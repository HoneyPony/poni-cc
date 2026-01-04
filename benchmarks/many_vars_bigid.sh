hyperfine \
    '../target/release/poni-cc-cli -i many_vars_bigid.c -o out/poni-many_vars_bigid.o --fun --no-cpp && clang -fuse-ld=mold out/poni-many_vars_bigid.o -o out/poni-many_vars_bigid' \
    'tcc many_vars_bigid.c -o out/tcc-many_vars_bigid'