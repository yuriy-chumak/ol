echo "
#include <stdio.h>

int main() {
    int c; FILE *f;
    f = fopen(\"fasl/bootp.fasl\", \"rb\");
    printf(\"char* language = \\\"\");
    while((c = fgetc(f)) != EOF)
        printf(\"\\\\x%02X\", (unsigned)c);
    printf(\"\\\";\");
    fclose(f);
    return 0;
}" | gcc -o Debug/xxd -xc -
Debug/xxd <fasl/boot.fasl >src/boot.c
