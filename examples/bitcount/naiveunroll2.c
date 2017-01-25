#include <stdio.h>

int bitcount(int input);

inline int bitcount (int in){
	int output = 0;
	for(int i = 0;i<8*sizeof(in);i+=2){
		if(in & (1 << i)) output++;
		if(in & (2 << i)) output++;
	}
	return output;
}

int main (int argc, int *argv[]){
	printf("%d\n",bitcount(11));
	return 0;
}
