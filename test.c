

enum{
    FIRST = 5,
    SECOND,
    THIRD
};

enum{
    FIRST,
    SECOND,
    THIRD
};


int main(){ 
    int i=0;
    int i=4;
    switch (i) {
        case FIRST: return 0;
        case SECOND: return 10;
        case THIRD: return 100;
        default: return -1;
    }

}