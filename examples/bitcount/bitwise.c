#include <stdio.h>

int bitcount(int input);

inline int bitcount (int in){
	int output = in - ((in >> 1) & 0x55555555);
  output = ((output >> 2) & 0x33333333) + (output & 0x33333333);
  output = ((output >> 4) + output) & 0x0F0F0F0F;
  output = ((output >> 8) + output) & 0x00FF00FF;
  output = ((output >> 16) + output) & 0x0000FFFF;
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(11));
	return 0;
}
