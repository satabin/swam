int main() {
    int a = 10;
    int d = 2 ;
    int b = 2,c = 1;
    if(a % 10 == 0)
        a = b + c; // ------------ instructions - 56 instructions ----- executed
    else
        d = b * c; // ------------ instructions - 13 instructions ----- not executed

    // instructions -------------- 5 instructions --------- executed
}

/* No of instruction in wasm counted manually :
Instructions per Method manual count:
Method Name --------------- Count------- Executed -------Not Executed 
__original_main				74            61                13
*/
