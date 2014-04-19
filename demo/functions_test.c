
// string str_fun(string a);
// int int_fun();
// double double_fun(string a);

string str_fun(string a) {
    print("str_fun");
    return a;
}

int int_fun() {
    print("int_fun");
    return 1;
}

double double_fun(string a) {
    print("double_fun");
    return 1.2;
}

int main() {
    string a;
    int b;
    double c;
    
    a = str_fun("");
    b = int_fun();
    c = double_fun("");
}