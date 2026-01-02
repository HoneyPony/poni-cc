#include <stdio.h>

int
main() {
    FILE *f = fopen("./return_lots_of_ones.c", "w");
    if(!f) return -1;

    fputs("int main(void) {\n\treturn 1", f);
    for(int i = 0; i < 1000000; ++i) {
        fputs("\n\t+ 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1", f);
    }
    fputs(";\n}\n", f);

    fclose(f);
    return 0;
}