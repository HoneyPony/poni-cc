int main(void) {
    // This currently compiles, even though it shouldn't.
    //
    // We might need to either ensure we aren't allowed to assign to temporary
    // variables, or do some other thing. (Maybe mark each temporary variable
    // as a valid lvalue? That doesn't quite work either, though...)
    //
    // Another option would be to treat user variable references as implicitly
    // (*&a), essentially, and then require that we are essentially doing a
    // pointer dereference in order to have an lvalue...? Idk...
    (3 + 5) = 20;
    return 10;
}