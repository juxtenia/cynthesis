int const lookup[] = {1,6,3,8,1,8,3,5};

int lookupme (int,int);

inline int lookupme (int a, int b){
    return lookup[a & 7] + lookup[b & 7];
}

int main (int argc, char *argv[]){
    return lookupme(argc,argc);
}
