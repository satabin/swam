int main() {
    int a = 10;
    int d = 2 ;
    int b = 2,c = 1;
    if((a % 10) == 1) // ------------ instructions - 56 instructions ----- executed
        a = b + c;    // ------------ instructions - 14 instructions ----- not executed 
    else
        d = b * c;   // ------------ instructions - 13 instructions -----  executed

    // instructions -------------- 5 instructions --------- executed
}

/* No of instruction in wasm counted manually :
Method Name --------------- Count------- Executed -------Not Executed 
__original_main				89            75                14
*/