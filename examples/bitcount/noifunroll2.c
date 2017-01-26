#include <stdio.h>

int bitcount(unsigned int input);

inline int bitcount (unsigned int input){
	int output = 0;
	for(int i = 0;i<8*sizeof(input);i+=2){
		int tmp = (input >> i);
		output+= (tmp & 1) ;
		output+= ((tmp >> 1) & 1);
	}
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(11));
	return 0;
}
