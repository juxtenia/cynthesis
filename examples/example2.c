int fib(int y){
	int a = 0, b = 1;
	for(int i = 0; i<y; i++){
		int tmp = b;
		b += a;
		a = tmp; 
	}
	return b;
}

int main ( int argc, int *argv[]){
	int d = fib(argc);
	return d;
}