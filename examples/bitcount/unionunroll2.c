#include <stdio.h>

int bitcount(unsigned int input);

inline int bitcount (unsigned int in){
	int lookup[4] = {0,1,1,2};
	union {
		int v;
		struct {
			unsigned int  i1:2;
			unsigned int  i2:2;
			unsigned int  i3:2;
			unsigned int  i4:2;
			unsigned int  i5:2;
			unsigned int  i6:2;
			unsigned int  i7:2;
			unsigned int  i8:2;
			unsigned int  i9:2;
			unsigned int i10:2;
			unsigned int i11:2;
			unsigned int i12:2;
			unsigned int i13:2;
			unsigned int i14:2;
			unsigned int i15:2;
			unsigned int i16:2;
		} s;
	} u;
	u.v=in;
	int output = 0;
	output += lookup[u.s.i1];
	output += lookup[u.s.i2];
	output += lookup[u.s.i3];
	output += lookup[u.s.i4];
	output += lookup[u.s.i5];
	output += lookup[u.s.i6];
	output += lookup[u.s.i7];
	output += lookup[u.s.i8];
	output += lookup[u.s.i9];
	output += lookup[u.s.i10];
	output += lookup[u.s.i11];
	output += lookup[u.s.i12];
	output += lookup[u.s.i13];
	output += lookup[u.s.i14];
	output += lookup[u.s.i15];
	output += lookup[u.s.i16];
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(11));
	return 0;
}
