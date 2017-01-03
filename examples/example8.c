union s {int i; long int j;};

int called(union s a){
	return a.i;
}

int main (int argc, int *argv[]){
	union s a;
	a.j=0;
	return called(a);
}