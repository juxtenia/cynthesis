#include <stdio.h>

int bitcount(unsigned int input);

inline int bitcount (unsigned int in){
	int output = 0;
	for(int i = 0;i<8*sizeof(in);i++){
		if(in & (1 << i)) output++;
	}
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(11));
	return 0;
}
