int const lookup[] = {11,14,0,101};

int constlookup();

inline int constlookup(){
    return lookup[1] + lookup[3];
}

int main (int argc, char *argv[]){
    return constlookup();
}
