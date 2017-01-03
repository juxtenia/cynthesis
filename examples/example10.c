struct s { unsigned int bit1 :1; unsigned int bit4 :4;};

typedef struct s bits;

int callee (bits b){
	return b.bit1 ? 3 : 4;
}

int main (int argc, int *argv[]){
	bits b = { 1, 15 };
	return callee(b);
}