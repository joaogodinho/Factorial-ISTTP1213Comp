#include <stdio.h>

char *copy(char *dest, char *source) {
    int ptrPos;
    for (ptrPos = 0; source[ptrPos] != '\0'; ptrPos++) {
        dest[ptrPos] = source[ptrPos];
        printf("%c\n", source[ptrPos]);
    }
    printf("%d\n", ptrPos);
    dest[ptrPos] = '\0';
    return dest;
}


int main() {
    char dest[20];
    copy(dest, "hello");
    printf("%s\n", dest);
}
