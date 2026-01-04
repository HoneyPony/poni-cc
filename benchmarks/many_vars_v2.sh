hyperfine \
    '../target/release/poni-cc-cli -i many_vars_v2.c -o out/poni-many_vars_v2.o --fun --no-cpp && clang -fuse-ld=mold out/poni-many_vars_v2.o -o out/poni-many_vars_v2' \
    'tcc many_vars_v2.c -o out/tcc-many_vars_v2'