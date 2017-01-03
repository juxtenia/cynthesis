struct s {int i; long int j;};

int main ( int argc, int *argv[]){
	struct s a = {0, 1};
	return sizeof (struct s);
}