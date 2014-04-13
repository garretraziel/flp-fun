int test(int a){
	return a;
}

int main(){
	int a;
	scan(a);
	if(a!=5){
		print(a);
	} else {
                a = test(2);
		print(a);
	}
	return 0;
}
