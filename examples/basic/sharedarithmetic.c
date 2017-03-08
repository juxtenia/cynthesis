int thing(int,int,int);

inline int thing(int x, int y, int z){
    int a = (x+y)+z;
    int b = (x+y)+z;
    return a*b;
}

int main(int argc, char *argv[]){
    return thing(argc,argc,argc);
}
