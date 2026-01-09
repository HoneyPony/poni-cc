//! The parser for poni-cc.
//! 
//! The main goal is to primarily operate through syntax-directed translation;
//! in particular, a single-pass compiler *should* be reasonably fast, which
//! is one of the main goals of this particular project.
//! 
//! This does mean we combine all three of:
//! - Parsing
//! - Semantic analysis (typechecking)
//! - Code generation
//! 
//! Which is a little ugly. But on the other hand, it is also sometimes quite
//! pleasant.
//! 
//! Of course, we would *also* like to provide an API for generating compiled
//! code from other applications. I'm not sure exactly how that should fit in;
//! perhaps the Parser calls into something else..?

use std::io::Read;

use ahash::HashMap;
use rustc_hash::FxHashMap;

use crate::{ctx::Ctx, lexer::{Lexer, StrKey, TokenType}};
use crate::ir::*;

/// A single scope of vars, e.g. there are two different VarScopes in the following:
/// ```c
/// int a = 30;
/// {
///     int a = 40; // valid
/// }
/// ```
struct VarScope {
    map: FxHashMap<StrKey, crate::ir::Var>,
}

impl VarScope {
    fn new() -> Self {
        VarScope {
            map: FxHashMap::default()
        }
    }
}

struct SwitchCases {
    /// Map of integer constants to labels. Note that we will likely need to
    /// change the structure of these in the future when we have more integer
    /// types.
    cases: HashMap<StrKey, Label>,
    /// Whether we have seen the default: case. If we have, then the switch
    /// statement should not put the label at the end. If we have not, the
    /// label should go at the end.
    seen_default: bool,

    /// The Label used for the default case. Stored here just so there's a little
    /// less enclosing_ juggling.
    default_label: Label,
}

impl SwitchCases {
    fn new(default_label: Label) -> Self {
        SwitchCases {
            cases: HashMap::default(),
            seen_default: false,
            default_label,
        }
    }
}

pub struct Parser<R: Read> {
    next_token: TokenType,
    next_token_two: TokenType,
    lexer: Lexer<R>,
    variables: Vec<VarScope>,
    /// Local labels in the current function. Cleared between functions.
    /// 
    /// Tuple: First item IR label, second item whether it was defined (or only
    /// seen in gotos)
    labels: HashMap<StrKey, (Label, bool)>,

    /// The Label, if any, that 'continue' should jump to. Updated when we
    /// parse/finish parsing a loop.
    current_continue: Option<Label>,
    /// The Label, if any, that 'break' should jump to. Updated when we
    /// parse/finish parsing a loop or switch.
    current_break: Option<Label>,

    current_cases: Option<SwitchCases>,
}

impl<R: Read> Parser<R> {
    pub fn new(input: R, ctx: &mut Ctx) -> Self {
        let mut result = Parser {
            next_token: TokenType::Eof,
            next_token_two: TokenType::Eof,
            lexer: Lexer::new(input, ctx),
            // Start with the global scope. (?)
            variables: vec![VarScope::new()],
            labels: HashMap::default(),

            current_continue: None,
            current_break: None,

            current_cases: None,
        };

        // Prime the parser so it has a legit token.
        result.advance(ctx);
        result.advance(ctx);
        result
    }

    fn advance(&mut self, ctx: &mut Ctx) -> TokenType {
        let result = self.next_token;
        self.next_token = self.next_token_two;
        self.next_token_two = self.lexer.next(ctx);

        result
    }

    fn expect(&mut self, ctx: &mut Ctx, typ: TokenType) -> TokenType {
        if self.next_token != typ {
            panic!("expected {} got {}", typ, self.next_token);
        }
        self.advance(ctx)
    }

    fn expect_id(&mut self, ctx: &mut Ctx) -> StrKey {
       let TokenType::Identifier(id) = self.next_token else {
            panic!("expected identifier got {}", self.next_token);
        };
        self.advance(ctx);
        id
    }

    fn match_(&mut self, ctx: &mut Ctx, typ: TokenType) -> bool {
        if self.next_token == typ {
            self.advance(ctx);
            return true;
        }
        false
    }

    fn lookup_var(&self, var_name: StrKey) -> Option<Var> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.map.get(&var_name) {
                return Some(*var);
            }
        }
        None
    }

    fn lookup_var_in_current_scope(&self, var_name: StrKey) -> Option<Var> {
        // NOTE: In theory we could "optimize" this in that we know we always
        // have a last so there's no need for the match. I'm not sure that
        // really matters.
        if let Some(scope) = self.variables.last() {
            scope.map.get(&var_name).copied()
        }
        else {
            None
        }
    }

    /// This function is currently unused. The idea is still something I want
    /// to come back to though: Basically, where possible, re-use existing
    /// variables when we're generating IR. This should result in many fewer
    /// redundant instructions, which is good.
    #[expect(unused)]
    fn promote_to_var(val: Val, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Var {
        match val {
            Val::Constant(_) => {
                let var = ctx.tmp().0;
                into.push(Instr::Copy { src: val, dst: var.into() });
                var

            },
            Val::RValue(str_id) => {
                str_id
            },
            Val::LValue(str_id) => {
                str_id
            }
            Val::Tmp(str_id) => { str_id }
        }
    }

    /// Returns Some() if we match a type, None otherwise. Used to parse
    /// the types in declarations in block items and for-loops.
    fn match_type(&mut self, ctx: &mut Ctx) -> Option<()> {
        // For now, we don't actually have any real types, so just return
        // a () if we match an 'int' keyword.
        self.match_(ctx, TokenType::Int).then_some(())
    }

    pub fn program(&mut self, ctx: &mut Ctx) -> Vec<Function> {
        let mut result = Vec::new();

        result.push(self.function(ctx));
        
        self.expect(ctx, TokenType::Eof);

        // TODO: We'll probably want to codegen functions as we go, not
        // push them all to a thing first.
        result
    }

    pub fn function(&mut self, ctx: &mut Ctx) -> Function {
        let mut instructions: Vec<Instr> = Vec::new();

        // New scope for the function's variables
        self.variables.push(VarScope::new());

        // Reset per-function labels
        self.labels.clear();

        self.expect(ctx, TokenType::Int);
        let ident = self.expect_id(ctx);
        self.expect(ctx, TokenType::LParen);
        self.expect(ctx, TokenType::Void);
        self.expect(ctx, TokenType::RParen);
        self.expect(ctx, TokenType::LBrace);

        while !matches!(self.next_token, TokenType::RBrace | TokenType::Eof) {
            self.block_item(ctx, &mut instructions);
        }

        self.variables.pop();

        self.expect(ctx, TokenType::RBrace);

        // If the function doesn't terminate with a return statement, add one.
        if !matches!(instructions.last(), Some(Instr::Return(_))) {
            instructions.push(Instr::Return(Val::Constant(ctx.zero())));
        }

        for label in self.labels.values() {
            if !label.1 {
                // We actually didn't save the name of the label. Oops.
                panic!("undefined label");
            }
        }

        // TODO: We probably want to make the ident.str just a property of
        // e.g. TokenType::Identifier, so that we don't have to call .unwrap()
        // here.
        Function { name: ident, body: instructions }
    }

    /// Finishes parsing a declaration with the given type. Places the variable
    /// into the innermost scope. Does NOT expect a semicolon at the end.
    fn finish_declaration(&mut self, ctx: &mut Ctx, _type: (), into: &mut Vec<Instr>) {
        // In the future, this will have to parse a whole type somehow...
        let var_name = self.expect_id(ctx);
        if self.lookup_var_in_current_scope(var_name).is_some() {
            panic!("duplicate variable '{}'", ctx.get(&var_name, &mut [0; 8]));
        }

        let var = ctx.var();
        // It should be safe to unwrap the last() because we always have
        // a global scope.
        self.variables.last_mut().unwrap().map.insert(var_name, var);

        // Parse initializer
        if self.match_(ctx, TokenType::Equal) {
            let initializer = self.expression(ctx, into);
            if let Val::Tmp(tmp) = initializer {
                // If our initializer was a (temporary) variable, steal its identity.
                //
                // We did already create our own identity, but this is OK:
                // it is undefined behavior if we referred to ourselves in
                // any way other than e.g. sizeof() in our initializer (at
                // least, I think that's the case).
                //
                // Maaaaybe something bad could happen with e.g. a pointer?
                // Maybe we're allowed to store a pointer to ourselves and
                // not have it be invalidated? In that case, I guess we
                // should actually count the number of references to the var,
                // and only do this if it was referenced 0 times so far. :shrug:
                //
                // I'll say TODO: fix that.
                self.variables.last_mut().unwrap().map.insert(var_name, tmp);
            }
            else {
                into.push(Instr::Copy { src: initializer, dst: var });
            }
        }
    }

    pub fn block_item(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) {
        if let Some(_type) = self.match_type(ctx) {
            // Declaration

            self.finish_declaration(ctx, _type, into);

            self.expect(ctx, TokenType::Semicolon);
        }
        else {
            self.statement(ctx, into);
        }
    }

    pub fn statement(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) {
        match self.next_token {
            TokenType::Return => {
                self.expect(ctx, TokenType::Return);
                let return_val = self.expression(ctx, into);
                self.expect(ctx, TokenType::Semicolon);

                // Generate the code for return
                into.push(Instr::Return(return_val));
            },
            TokenType::Semicolon => {
                // Skip empty statements.
                self.advance(ctx);
            }
            TokenType::If => {
                self.expect(ctx, TokenType::If);
                self.expect(ctx, TokenType::LParen);

                let condition = self.expression(ctx, into);
                
                self.expect(ctx, TokenType::RParen);

                let if_false = ctx.label("if_f");

                // If the condition is false, jump to the if_false label.
                into.push(Instr::JumpIfZero { condition, target: if_false });

                // Otherwise, perform the inner statement.
                self.statement(ctx, into);

                if self.match_(ctx, TokenType::Else) {
                    // If there is an else branch, then we need to jump to the
                    // end label at the end of the 'if' branch.
                    let if_end = ctx.label("if_end");
                    into.push(Instr::Jump(if_end));

                    // Now, we start the if_f label.
                    into.push(Instr::Label(if_false));
                    // And we generate the else code.
                    self.statement(ctx, into);
                    // Then the 'end' label will come after that.
                    into.push(Instr::Label(if_end));
                }
                else {
                    // If there was no else branch, then we simply generate the
                    // if_false label after the inner statement.
                    into.push(Instr::Label(if_false));
                }
            }
            TokenType::Break => {
                self.expect(ctx, TokenType::Break);

                let Some(label) = self.current_break else {
                    panic!("break must be inside a loop or switch");
                };

                into.push(Instr::Jump(label));
                self.expect(ctx, TokenType::Semicolon);
            }
            TokenType::Continue => {
                self.expect(ctx, TokenType::Continue);

                let Some(label) = self.current_continue else {
                    panic!("continue must be inside a loop");
                };

                into.push(Instr::Jump(label));
                self.expect(ctx, TokenType::Semicolon);
            }
            TokenType::While => {
                self.expect(ctx, TokenType::While);
                self.expect(ctx, TokenType::LParen);

                // Code structure:
                // begin-loop:
                //     c <= <evaluate condition>
                //     JumpIfZero c, end-loop
                //     <loop body>
                //     Jump begin-loop
                // end-loop:

                let begin_loop = ctx.label("while_b");
                let end_loop = ctx.label("while_e");

                // I should maybe consider breaking these into helper functions
                let enclosing_break = self.current_break;
                let enclosing_continue = self.current_continue;

                // Break goes to the end of the loop
                self.current_break = Some(end_loop);
                // Continue goes back to the begin of the loop
                self.current_continue = Some(begin_loop);

                into.push(Instr::Label(begin_loop));

                // Now we parse the condition.
                let condition = self.expression(ctx, into);
                into.push(Instr::JumpIfZero { condition, target: end_loop });

                self.expect(ctx, TokenType::RParen);

                // Parse the loop body.
                self.statement(ctx, into);

                // Add the jump to the beginning, and the end-loop label.
                into.push(Instr::Jump(begin_loop));
                into.push(Instr::Label(end_loop));

                self.current_break = enclosing_break;
                self.current_continue = enclosing_continue;
            }
            TokenType::Do => {
                // Do-while is a little tricky because continue can't jump
                // to the beginning of the loop, it has to jump to the conditional
                // expression.
                self.expect(ctx, TokenType::Do);

                let begin_loop = ctx.label("do_b");
                let continue_loop = ctx.label("do_c");
                let end_loop = ctx.label("do_e");

                // I should maybe consider breaking these into helper functions
                let enclosing_break = self.current_break;
                let enclosing_continue = self.current_continue;

                // Break goes to the end of the loop
                self.current_break = Some(end_loop);
                // Continue goes to the conditional expression.
                self.current_continue = Some(continue_loop);

                into.push(Instr::Label(begin_loop));

                // Parse loop body.
                self.statement(ctx, into);

                self.expect(ctx, TokenType::While);
                self.expect(ctx, TokenType::LParen);

                into.push(Instr::Label(continue_loop));
                // Parse condition.
                let condition = self.expression(ctx, into);
                // Push the JumpIfNotZero instruction. If the condition is
                // a known constant zero, don't bother.
                if !matches!(condition, Val::Constant(c) if c == ctx.zero()) {
                    into.push(Instr::JumpIfNotZero { condition, target: begin_loop });
                }

                // End of loop.
                into.push(Instr::Label(end_loop));

                self.expect(ctx, TokenType::RParen);
                self.expect(ctx, TokenType::Semicolon);

                self.current_break = enclosing_break;
                self.current_continue = enclosing_continue;
            }
            TokenType::For =>  {
                // Two possible structurs for a for loop, for our single-pass
                // compiler.
                // 1). No additional Vec<Instr> required. This setup requires
                // an extra Jump loop-begin instruction.
                //
                //     <initializer>
                //     Jump loop-begin
                // loop-expr:
                //     <loop-expr>
                // loop-begin:
                //     c = <condition>
                //     JumpIfZero c, loop-end
                //     <loop-body>
                //     Jump loop-expr
                // loop-end:
                //
                // 2). Use an additional Vec<Instr> for the loop-expr than
                // concatenate them at the end.
                //     
                //     <initializer>
                // loop-begin:
                //     c = <condition>
                //     JumpIfZero c, loop-end
                //     <loop-body>
                // loop-continue:
                //     <loop-expr>
                //     Jump loop-begin
                // loop-end:
                //
                // Note that in both cases we still need three labels. This
                // is because the continue expression must still execute the
                // loop-expr.
                //
                // I guess for now we'll go with 2 because it seems slightly
                // better.

                // Note that we have to push a new variable scope BEFORE the
                // for loop, because that's where its variable belongs.
                self.variables.push(VarScope::new());

                self.expect(ctx, TokenType::For);
                self.expect(ctx, TokenType::LParen);

                // The first item is either a declaration or an optional expression.
                if let Some(_type) = self.match_type(ctx) {
                    self.finish_declaration(ctx, _type, into);
                }
                else {
                    self.optional_expression(ctx, TokenType::Semicolon, into);
                }
                self.expect(ctx, TokenType::Semicolon);

                let begin_loop = ctx.label("for_b");
                let end_loop = ctx.label("for_e");
                let continue_loop = ctx.label("for_c");

                // I should maybe consider breaking these into helper functions
                let enclosing_break = self.current_break;
                let enclosing_continue = self.current_continue;

                self.current_break = Some(end_loop);
                self.current_continue = Some(continue_loop);

                into.push(Instr::Label(begin_loop));
                let condition = self.optional_expression(ctx, TokenType::Semicolon, into);
                self.expect(ctx, TokenType::Semicolon);
                // If we got a condition, we must jump to the end of the loop
                // if it's false.
                //
                // If there was no condition, it's an infinite loop, so just
                // don't bother generating a jump.
                if let Some(condition) = condition {
                    into.push(Instr::JumpIfZero { condition, target: end_loop })
                }

                let mut expr = Vec::new();
                self.optional_expression(ctx, TokenType::RParen, &mut expr);

                self.expect(ctx, TokenType::RParen);

                // Okay, now parse loop body.
                self.statement(ctx, into);

                into.push(Instr::Label(continue_loop));

                // Append the loop expression to the body.
                into.append(&mut expr);
                // Jump back to condition.
                into.push(Instr::Jump(begin_loop));
                into.push(Instr::Label(end_loop));

                self.current_break = enclosing_break;
                self.current_continue = enclosing_continue;

                self.variables.pop();
            }
            TokenType::Switch => {
                self.expect(ctx, TokenType::Switch);

                // For now, the way I'm going to do this is as follows:
                // - Collect instructions into a separate Vec<Instr>
                // - Generate the dispatch table at the top, in an O(n) search
                //
                // This avoids the extra jump down to a dispatch table at the
                // bottom, at the cost of having to buffer the switch statement
                // code in a separate Vec<> for a while.
                //
                // (One thing we could consider is, instead of appending the
                // Vec to build it up, instead build up a Vec-of-Vecs or other
                // sort of list of Vecs so that we don't need any additional
                // big copies. But hopefully it won't be too bad in practice.)

                self.expect(ctx, TokenType::LParen);
                let expr = self.expression(ctx, into);
                self.expect(ctx, TokenType::RParen);

                let label_default = ctx.label("switch_d");
                let label_end = ctx.label("switch_e");

                // Collect the code into a separate vec.
                let enclosing_cases = self.current_cases.take();
                let enclosing_break = self.current_break;

                self.current_cases = Some(SwitchCases::new(label_default));
                // Break jumps to the end of the switch statement.
                self.current_break = Some(label_end);

                // Switch is a little weird in that it is literally followed
                // by a single statement.
                //
                // This means that `case 0: x = 5;`, for example, is a single
                // statement.
                //
                // If we want to support C23 properly, we also want to treat
                // `case 0: int x = 5;` as a single statement, although I likely
                // won't bother for now.
                let mut code = Vec::new();
                // Parse that single statement.
                self.statement(ctx, &mut code);

                // Now that we have all the cases, we can generate the jump
                // table.
                let cases = self.current_cases.take().unwrap();
                // We need a separate temporary from our controlling expression
                // for the binary ops.
                //
                // (It would, of course, be ideal to just directly generate
                // the comparison instructions. Maybe in the future...)
                let dst = ctx.tmp();
                for (const_val, label) in cases.cases {
                    into.push(Instr::Binary {
                        op: BinaryOp::Equal,
                        dst: dst.0,
                        lhs: expr,
                        rhs: Val::Constant(const_val)
                    });

                    // Jump to this label if the comparison succeeded.
                    into.push(Instr::JumpIfNotZero { condition: dst.0.clone().into(), target: label })
                }
                // If we didn't jump to any label, jump to the default one.
                into.push(Instr::Jump(label_default));

                // Append the code to ourselves.
                into.append(&mut code);

                // If there wasn't a default label, put it at the end.
                if !cases.seen_default {
                    into.push(Instr::Label(label_default));
                }
                into.push(Instr::Label(label_end));

                self.current_cases = enclosing_cases;
                self.current_break = enclosing_break;
            }
            TokenType::Case => {
                self.expect(ctx, TokenType::Case);

                // TODO: We will need constant folding for this to work
                // completely correctly.
                let value = self.expression(ctx, into);
                let Val::Constant(c) = value else {
                    panic!("expression of case must be a constant");
                };

                self.expect(ctx, TokenType::Colon);

                let Some(cases) = self.current_cases.as_mut() else {
                    panic!("case must be inside a switch statement");
                };

                let label = ctx.label("case");
                if cases.cases.insert(c, label).is_some() {
                    panic!("duplicate case '{}'", ctx.get(&c, &mut [0; 8]));
                }

                // Now we can jump to this label.
                into.push(Instr::Label(label));

                // Now we must recurse into another statement. This is so that
                // e.g. switch(123) case 1: x = 5; is a valid program.
                self.statement(ctx, into);
            }
            TokenType::Default => {
                self.expect(ctx, TokenType::Default);
                self.expect(ctx, TokenType::Colon);

                let Some(cases) = self.current_cases.as_mut() else {
                    panic!("default must be inside a switch statement");
                }; 

                if cases.seen_default {
                    panic!("duplicate default in switch");
                }

                cases.seen_default = true;
                into.push(Instr::Label(cases.default_label));

                // Same as with case.
                self.statement(ctx, into);
            }
            // Label
            TokenType::Identifier(label) if matches!(self.next_token_two, TokenType::Colon) => {
                // Eat the id and the colon
                self.expect_id(ctx);
                self.expect(ctx, TokenType::Colon);

                // To do this right, we're gonna want to do it like this:
                // - If we see the goto before the label, insert something like
                //   (ir_label, false)
                // - Set the second member of the tuple to true in the label
                // - Error if it was already true
                //
                // We can check if there's an undefined label (i.e. used in goto,
                // not defined in the function() by iterating through the hashmap
                // at the end.
                let (ir_label, _) = *self.labels.entry(label)
                    .and_modify(|(_, defined)| {
                        if *defined {
                            panic!("duplicate label '{}'", ctx.get(&label, &mut[0; 8]))
                        }
                        // Got the label
                        *defined = true;
                    })
                    // If we're inserting it it's definitely defined
                    .or_insert_with(|| (ctx.label("c"), true));
                into.push(Instr::Label(ir_label));

                // To follow the C grammar, we actually want to treat this as
                // one statement, and recurse into another statement.
                self.statement(ctx, into);
            }
            TokenType::Goto => {
                self.expect(ctx, TokenType::Goto); // Eat goto
                let label = self.expect_id(ctx);
                let (ir_label, _) = *self.labels.entry(label)
                    // If we had to make the IR label from the goto, it might
                    // end up being undefined.
                    .or_insert_with(|| (ctx.label("c"), false));
                into.push(Instr::Jump(ir_label));

                self.expect(ctx, TokenType::Semicolon);
            }
            TokenType::LBrace => {
                self.expect(ctx, TokenType::LBrace);
                self.variables.push(VarScope::new());
                loop {
                    if matches!(self.next_token, TokenType::RBrace | TokenType::Eof) { break; }

                    // It is important to use block_item() and not statement()
                    // here.
                    self.block_item(ctx, into);
                }
                self.variables.pop();
                self.expect(ctx, TokenType::RBrace);

                // Don't expect semicolon.
            }
            _ => {
                self.expression(ctx, into);
                self.expect(ctx, TokenType::Semicolon);
            }
        }
        
    }

    pub fn expression(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        self.climb_precedence(ctx, into, 0)
    }

    fn optional_expression(&mut self, ctx: &mut Ctx, follow: TokenType, into: &mut Vec<Instr>) -> Option<Val> {
        if self.next_token == follow {
            return None;
        }
        Some(self.expression(ctx, into))
    }

    fn next_token_precedence(&self) -> Option<i32> {
        // See:
        // https://en.cppreference.com/w/c/language/operator_precedence.html
        match self.next_token {
            // Comma => 0
            TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                    TokenType::StarEqual | TokenType::SlashEqual | TokenType::PercentEqual |
                    TokenType::AmpersandEqual | TokenType::PipeEqual |
                    TokenType::CaretEqual |
                    TokenType::LessLessEqual | TokenType::GreaterGreaterEqual => Some(5),
            TokenType::Question => Some(10),
            TokenType::PipePipe => Some(15),
            TokenType::AmpersandAmpersand => Some(20),
            TokenType::Pipe => Some(25),
            TokenType::Caret => Some(30),
            TokenType::Ampersand => Some(35),
            TokenType::EqualEqual | TokenType::BangEqual => Some(40),
            TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => Some(45),
            TokenType::LessLess | TokenType::GreaterGreater => Some(50),
            TokenType::Plus | TokenType::Minus => Some(55),
            TokenType::Star | TokenType::Slash | TokenType::Percent => Some(60),
            _ => None
        }
    }

    pub fn climb_precedence(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>, min_prec: i32) -> Val {
        let mut lhs = self.factor(ctx, into);

        while let Some(prec) = self.next_token_precedence() && prec >= min_prec {
            let op = self.advance(ctx);

            // Handling for short circuiting operators is special.
            if matches!(op, TokenType::AmpersandAmpersand | TokenType::PipePipe) {
                // TODO: We can probably do this in a better way, with maybe
                // a couple extra IR instructions.
                //
                // The approach from the book requires two labels, and one
                // unconditional jump in the false case. It looks like:
                // v1 = <eval e1>
                // JumpIfZero(v1, false_label)
                // v2 = <eval e2>
                // JumpIfZero(v2, false_label)
                // result = 1
                // Jump(end)
                // Label(false_label)
                // result = 0
                // Label(end)
                //
                // But it should be possible to do fewer jumps and labels using
                // something like:
                // v1 = <eval e1>
                // result = !!v1
                // JumpIfZero(result, end)
                // v2 = <eval e2>
                // result = !!v2
                // Label(end)
                //
                // In some cases, this !!v1 could even just directly evaluate
                // the condition flags, which would be nice.
                //
                // Anyway, for now we'll do it the inefficient way because it's
                // straightforward to implement.

                // Note to self: For ||, if we use the same approach as the book,
                // we will want to do:
                // v1 = <eval e1>
                // JumpIfNotZero(v1, true_label)
                // v2 = <eval e2>
                // JumpIfNotZero(v2, true_label)
                // result = 0
                // Jump(end)
                // Label(true_label)
                // result = 1
                // Label(end)

                let is_or = matches!(op, TokenType::PipePipe);

                let tf_label = ctx.label(if is_or { "true" } else { "false" });
                let end_label = ctx.label("end");
                // Right now, we have the lhs evaluated. So, we need to jump_if_zero
                // to our false label.
                into.push(if is_or {
                    Instr::JumpIfNotZero { condition: lhs, target: tf_label }
                } else {
                    Instr::JumpIfZero { condition: lhs, target: tf_label }
                });

                // Now we evaluate the rhs. That way, it is NOT evaluated if
                // we had jumped.
                let rhs = self.climb_precedence(ctx, into, prec + 1);
                into.push(if is_or {
                    Instr::JumpIfNotZero { condition: rhs, target: tf_label }
                } else {
                    Instr::JumpIfZero { condition: rhs, target: tf_label }
                });


                // Create the true value.
                let dst = ctx.steal_tmp(lhs);
                into.push(Instr::Copy { src: Val::Constant(if is_or { ctx.zero() } else { ctx.one() }), dst: dst.0 });
                // Jump to the end.
                into.push(Instr::Jump(end_label));

                // Create the false value.
                into.push(Instr::Label(tf_label));
                into.push(Instr::Copy { src: Val::Constant(if is_or { ctx.one() } else { ctx.zero() }), dst: dst.0 });
                into.push(Instr::Label(end_label));

                // Make sure to update lhs!
                lhs = dst.into();
            }
            else if matches!(op,
                TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                    TokenType::StarEqual | TokenType::SlashEqual | TokenType::PercentEqual |
                    TokenType::AmpersandEqual | TokenType::PipeEqual |
                    TokenType::CaretEqual |
                    TokenType::LessLessEqual | TokenType::GreaterGreaterEqual
            ) {
                // Assignment
                
                // Assignment is right associative, so use prec instead of
                // prec + 1
                let rhs = self.climb_precedence(ctx, into, prec);

                if let Val::LValue(var) = lhs {
                    if op == TokenType::Equal {
                        into.push(Instr::Copy { src: rhs, dst: var });
                    }
                    else {
                        // This will probably be a bit tricky with other kinds
                        // of lvalues. For now, just generate a BinaryOp that
                        // is assigning to the destination.
                        into.push(Instr::Binary { op: BinaryOp::from(op), dst: var, lhs, rhs })
                    }

                    // The LHS doesn't change super meaningfully here, but
                    // it DOES need to no longer be an lvalue.
                    lhs = lhs.to_rvalue();
                }
                else {
                    // TODO:
                    // To properly handle LValues, we miiiight need an AST?
                    // Maybe not.
                    panic!("assignment to non-lvalue");
                }
            }
            // Ternary operator
            else if matches!(op, TokenType::Question) {
                // To properly handle this, we must short circuit evaluation.
                // We've already evaluated the condition, i.e. LHS, which is
                // fine. But now we have to skip *either* of the branches if
                // they aren't taken.
                //
                // So, we start by jumping to the else/false branch.
                let label_else = ctx.label("tern_f");
                into.push(Instr::JumpIfZero { condition: lhs, target: label_else });

                // Parse the true branch. This is just a regular expression()
                // call, because it's sort of "internal" to the operator.
                let true_val = self.expression(ctx, into);
                let dst = ctx.steal_tmp(true_val);

                // Copy the true_val into our dst.
                into.push(Instr::Copy { src: true_val, dst: dst.0 });
                // Jump over the false branch.
                let label_end = ctx.label("tern_end");
                into.push(Instr::Jump(label_end));

                self.expect(ctx, TokenType::Colon);

                // Now we parse the false branch.
                // We use prec instead of prec + 1 as this operator is
                // right-associative.
                
                // The false label comes before this code.
                into.push(Instr::Label(label_else));
                let false_val = self.climb_precedence(ctx, into, prec);
                into.push(Instr::Copy { src: false_val, dst: dst.0 });

                // Done.
                into.push(Instr::Label(label_end));

                lhs = dst.into();
            }
            else {
                let op = BinaryOp::from(op);
                let rhs = self.climb_precedence(ctx, into, prec + 1);
                // We are allowed to steal the lhs but not the rhs. I'm not
                // entirely sure why that is...
                let dst = ctx.steal_tmp(lhs);

                into.push(Instr::Binary { op, dst: dst.0, lhs, rhs });
                lhs = dst.into();
            }
        }

        lhs
    }

    /// Like atom, but with postfix operators.
    /// 
    /// We may change how this works in the future, to support multiple postfix
    /// operators? Maybe just have that logic also in climb_precedence?
    pub fn factor(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        let atom = self.atom(ctx, into);

        if matches!(self.next_token, TokenType::PlusPlus | TokenType::MinusMinus) {
            let op = self.advance(ctx);

            let Val::LValue(var) = atom else {
                panic!("expected lvalue");
            };

            let tmp = ctx.tmp();
            into.push(Instr::Copy {
                dst: tmp.0,
                src: atom
            });

            into.push(Instr::Binary {
                op: BinaryOp::from(op),
                dst: var,
                lhs: atom,
                rhs: Val::Constant(ctx.one())
            });

            // We keep the value before the increment.
            return tmp.into();
        }

        return atom;
    }

    pub fn atom(&mut self, ctx: &mut Ctx, into: &mut Vec<Instr>) -> Val {
        match self.next_token {
            TokenType::Constant(value) => {
                self.advance(ctx);
                // Same thing as above (we should delete the unwrap...)
                //
                // Also, TODO: Validate that this StrId is a valid integer constant,
                // and/or convert it if necessary.
                Val::Constant(value)
            },
            TokenType::Identifier(var_name) => {
                self.advance(ctx);

                let Some(var) = self.lookup_var(var_name) else {
                    panic!("unresolved identifier '{}'", ctx.get(&var_name, &mut [0; 8]));
                };

                // Identifiers produce an LValue.
                //
                // Note that this still works properly even when we e.g.
                // wrap the identifier in parenthesis.
                Val::LValue(var)
            }
            // Unary operators
            op @ (TokenType::Tilde | TokenType::Minus | TokenType::Bang) => {
                self.advance(ctx);

                // NOTE: There's actually no real reason to use a new tmp
                // here? We should probably just overwrite the src variable.
                //
                // Some thoughts on why there is a new tmp here:
                // If we didn't have a new tmp, we could end up generating
                // something like negl $123, which is obviously wrong.
                //
                // ...But, there is no reason to generate *any code at all*
                // in that case. I will probably end up accidentally implementing
                // a constant propogator + dead code eliminator or something?
                // 
                // Probably I want some sort of function like 'take this Val and
                // make it real'
                //
                // E.g. I guess it's not the end of the world if we end up
                // generating movl $123, %edi ; negl %edi ; etc. Of course it
                // feels like we are really just missing some sort of constant
                // propogator.
                //
                // (Actually, maybe more particularly... If we DO end up
                // generating Instr::Unary { op, reg: Val::Constant }), then
                // what we'd like to do is simply replace the constant val
                // there with the evaluated value. E.g. have something like
                // re-evaluate-this-constant-please. IDK. It's not exactly
                // clear).

                // src must be evaluated first
                let src = self.factor(ctx, into);

                // This might work if it were 'promote_to_temporary_var'
                // instead. But for now, we will just always create a new
                // dst var. This is less efficient, but we have ways to deal
                // with that (?)
                // let dst = Self::promote_to_var(src, ctx, into);

                let op = UnaryOp::from(op);
                let dst = ctx.steal_tmp(src);
                into.push(Instr::Unary { op, dst: dst.0, src });

                dst.into()
            },
            op @ (TokenType::PlusPlus | TokenType::MinusMinus) => {
                self.advance(ctx);

                let target = self.factor(ctx, into);
                let Val::LValue(var) = target else {
                    panic!("expected lvalue");
                };

                into.push(Instr::Binary {
                    op: BinaryOp::from(op),
                    dst: var,
                    lhs: target,
                    rhs: Val::Constant(ctx.one())
                });

                // We keep the same value as we just created, except it cannot
                // be an LValue anymore.
                target.to_rvalue()
            }
            TokenType::LParen => {
                self.advance(ctx);

                let result = self.expression(ctx, into);

                self.expect(ctx, TokenType::RParen);
                result
            }
            _ => {
                panic!("expected expression, got '{}'", self.next_token.to_str_const());
            }
        }
    }
}