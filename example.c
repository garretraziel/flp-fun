// globalni promenne
double PI;

/***** sekce deklarace funkci *****/
int factorial(int n);

string uvitani(string a, int b) {
	print("Dobry den "+a);
	print("Faktorial zadaneho cisla je:");
        print(factorial(b));
	print("A hodnota PI je:");
	print(PI);
        return a;
}

int factorial(int n)
{
	int result;
	if (n < 2) {
		return 1;
	} else {
		return n * factorial(n-1);
	}
}

int main()
{
	string a;
        int b;
	PI = 3.141592;
	print("Zadejte vase jmeno:");
	scan(a);
        print("Zadejte cislo pro vypocet faktorialu:");
	scan(b);
	if (b < 0) {
		print("Faktorial nelze spocitat.");
	} else {
		uvitani(a, b);
	}
	return 0;
}
