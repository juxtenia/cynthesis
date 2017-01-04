#include <stdio.h>

int main (int argc, int *argv[]){
    unsigned char uc = 255;
    signed char sc = (signed char) uc;
    printf("%d\n",sc);
    return 0;
}
