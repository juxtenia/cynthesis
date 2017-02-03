inline int twoloops (int in){
    int output = 0;
    for(int i = 0; i<in; i++){
        output += i;
    }
    for(int j = 0; j<in; j++){
        output += j*j;
    }
}

int main(int argc, char* argv[]){
    return twoloops(argc);
}
