#include <stdio.h>

int main()
{
    int i,j=10;
    for(i=1; i<=5; i++)
    {
        int a = i + j;
    }
    return 0;
}

/* No of instruction in wasm counted manually :
Method Name --------------- Count------- Executed
__original_main				75            75       
*/