/*
 * THIS ONE IS MAGIC:
 * ked necham len jednu deklaraciu a priradenie tak to ide, ale
 * v momente kedy je za assignom ďalší kód tak sa to zloží
 */


int main(){   
    double aa;
    double bb;
    double cc;
    double dd;
    
    //double  operations
    
    bb = 4.0;
    aa= 5;      //BUG: it breaks here: xmokra00: Cannot assing to variable aa: Bad type
    bb= -4;
    cc= 12;
    dd= 0;
}