int func_1(int a);
int fact(int a);
int printing(string str, int a);

/************ MAIN ************/
int main() {
    int x;
    int res;
    string str;
    
    x = func_1(2);
    
    res = fact(x);
    
    printing("Hi", res); //BUG here
}

int func_1(int a) {
    return 45;
}

int fact(int a) {
    if (a >0) {
        return a * (a - 1);
    } else {
        return 1;
    }
}


int printing(string str, int a) {
    double num;
    
    print(str);
    print(a);
    print(a/4.0);
    print(a/(4.0*0.5));
    print("Zadajte"+" double");
    scan(num);
    if(num > "lorem ipsum") {
        print("výsledok po delení 0.333");
        print(num/0.333);
    } else {
        print("výsledok po delení 0.25");
        print(num/0.25);
    }
}