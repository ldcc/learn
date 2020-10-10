#include <stdio.h>

//#pragma pack(4)
struct A
{
    char               a;
    int                b;
    unsigned short     c;
    long               d;
    unsigned long long e;
    char               f;
}__attribute__((aligned (16)))A;
//#pragma pack()

int
main()
{
    printf("sizeof(TEST) = %lu\n", sizeof(A));
    return 0;
}