
int factorial(int a);

int factorial(int a){
    if(a>1) {
        return a * factorial (a-1);
    } else {
        return 1;
    }
}

int main(){
	int a;
        int result;
        
	a=5;
        
        if(a>0){
            result =factorial(a); //BUG HERE
        } else {
            result= -1;
        }
        
        if ( result > 0 ) {
            print("result is: ");
            print(result);
        } else {
            print("input is lower than zero!");
        }
}

