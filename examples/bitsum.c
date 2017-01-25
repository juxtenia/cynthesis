#include <stdio.h>

int bitcount(int input);

inline int bitsum (int in){
	union {
		int v;
		struct {
			unsigned char c1;
			unsigned char c2;
			unsigned char c3;
			unsigned char c4;
		} s;
	} u;
	u.v=in;
	return u.s.c1 + u.s.c2 + u.s.c3 + u.s.c4;
}

int main (int argc, int *argv[]){
	
	int i = (11<<24)+(10<<16)+(9<<8)+argc;
	printf("%d\n",bitsum(i));
	return 0;
}
