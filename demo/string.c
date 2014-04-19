
int factorial(int a);

int factorial(int a){
    if(a>1) {
        return a * factorial (a-1);
    } else {
        return 1;
    }
}

int main(){
	string input;
        int result;
        double test;
        
        test = 2.5;
        
        print("Napíšte niečo!");
        scan(input);
        print("Napísali ste:");
        print(input);
        print("*************************");
        print((factorial(5)+test)/0.5);
        print("string concat test:");
        print("hello" +" "+ "world"+" "+input);
        print("*************************");
        
}