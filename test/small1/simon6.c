
#define ASZ 10

extern void printf(char *fmt, ...);

void main(int argc, char **argv)
{
		int n = argc;
		int i;
		int a[ASZ];
		int *p = argc%2 ? 0 : a;

		a[0] = 10;
		printf("%d\n", p[0]);
		//printf("%d %d\n", p[0], p[1]);

		
		for(i=0; i<ASZ; ++i) {
				a[i] = (i*3 + 1) % ASZ;
				printf("[%d:%d]", i, p[i]);
		}

}
