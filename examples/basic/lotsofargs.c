inline int foo (int i, int j, int k, int l, int m){
    int x = (i+j) + k;
    int y = (i+j) + l;
    return m ? x : y;
}

int main (int argc, char *argv[]){
    return foo(argc + 1,argc + 2,argc + 3,argc + 4,argc + 5);
}
