/* Program 2: Vypocet faktorialu (rekurzivne) */
int factorial(int n);

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
    int a;
    int vysl;
    print("Zadejte cislo pro vypocet faktorialu: ");
    scan(a);
    if (a < 0) {
        print("Faktorial nelze spocitat.\n");
    } else {
        vysl = factorial(a);
        print("Vysledek je: ");
        print(vysl);
        print("\n");
    }
}