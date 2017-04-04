#include <stdio.h>

int bitcount(unsigned int input);

inline int bitcount (unsigned int in){
	// Borrowed from the following, which explains why this works:
	// http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel 
	int output = in - ((in >> 1) & 0x55555555);
	output = ((output >> 2) & 0x33333333) + (output & 0x33333333);
	output = ((output + (output >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(argc));
	return 0;
}
