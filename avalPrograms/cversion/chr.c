#include <stdio.h>

char *chr(char *str, int character) {
    int ptrPos = 0;
    do { } while (*(str+ptrPos++) != (char)character);
    return str+ptrPos-1;
}

int main() {
    char *str = "hello";
    printf("%c\n", *(chr(str, 'h')));
}
