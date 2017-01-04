struct s { unsigned int bit1 :1; unsigned int bit4 :4;};

union u { struct s ans; int i; };

typedef union u bits;

int callee (bits b){
	return b.ans.bit1 ? 3 : 4;
}

int main (int argc, int *argv[]){
	bits b;
	b.i = 5;
	return callee(b);
}
