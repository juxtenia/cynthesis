struct s {int i; long int j;};

int called(struct s a){
	return a.i;
}

int main (int argc, int *argv[]){
	struct s a = {0, 1};
	return called(a);
}