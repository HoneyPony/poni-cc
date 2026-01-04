#include <stdio.h>

int
main() {
    FILE *f = fopen("./many_vars_bigid.c", "w");
    if(!f) return -1;

    fputs("int main(void) {\n", f);
    for(int i = 0; i < 1000000; ++i) {
        fprintf(f, "\n\tint abcdabcdabcdabcd%d = %d;", i, i);
    }
    fputs(";\n}\n", f);

    fclose(f);
    return 0;
}