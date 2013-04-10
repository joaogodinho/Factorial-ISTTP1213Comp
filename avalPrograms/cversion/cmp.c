#include <stdio.h>

int compare(char *str1, char *str2) {
    int ptrPos = 0;
    char *ptr1, *ptr2;

    // C ONLY, IF STRINGS ARE EQUAL, IT'S THE SAME ADDRESS, CHECK IT
    if (str1 == str2) { return 0; }
    ptr1 = str1;
    ptr2 = str2;

    do { } while (*(ptr1+ptrPos) == *(ptr2+ptrPos++));
    ptrPos--;
    if ( *(ptr1+ptrPos) > *(ptr2+ptrPos)) { return 1; }
    else if ( *(ptr1+ptrPos) < *(ptr2+ptrPos)) { return -1; }
    return 0;
}

int main() {
    int a = compare("Hello", "Hello");
    printf("%d\n", a);
}
