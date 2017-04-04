int addone (int x) { return x + 1; }

int foo (int x);

inline int foo (int x) { return addone(addone(x)); }

int main (int argc, char *argv[]){
    return foo(argc);
}
