#include <stdio.h>

int combine(char, char, char, char);

inline int combine(char c1, char c2, char c3, char c4){
    union {
        int i;
        struct {char c1; char c2; char c3; char c4;} s;
    } u;
    u.s.c1=c1;
    u.s.c2=c2;
    u.s.c3=c3;
    u.s.c4=c4;
    return u.i;
}

int main(int argc, char *argv[]){
    int i = combine((char)argc,(char)1,(char)2,(char)0); 
    printf("%d\n",i);
    return i;
}
