hyperfine \
    '../target/release/poni-cc-cli -i many_vars.c -o out/poni-many_vars.o --fun --no-cpp && clang -fuse-ld=mold out/poni-many_vars.o -o out/poni-many_vars' \
    'tcc many_vars.c -o out/tcc-many_vars'