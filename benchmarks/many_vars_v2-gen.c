#include <stdio.h>

int
main() {
    FILE *f = fopen("./many_vars_v2.c", "w");
    if(!f) return -1;

    fputs("int main(void) {\n", f);
    for(int i = 0; i < 1000000 / 3; ++i) {
        fprintf(f, "\n\tint a%d = %d;", i, i);
        fprintf(f, "\n\tint b%d = %d;", i, i);
        fprintf(f, "\n\tint c%d = a%d + b%d;", i, i, i);
    }
    fputs(";\n}\n", f);

    fclose(f);
    return 0;
}