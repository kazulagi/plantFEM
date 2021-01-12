#include <stdio.h>

int checkaddress(){
    int hoge = 5;
    int piyo = 10;
    int *hoge_p;

    // Display address of each variables 
    printf("&hoge -> %p\n",(void*)&hoge);
    printf("&piyo -> %p\n",(void*)&piyo);
    printf("&hoge_p -> %p\n",(void*)&hoge_p);
    
    // Input address of hoge into hoge_p

    hoge_p=&hoge;
    printf("&hoge_p -> %p\n",(void*)&hoge_p);

    // Display the value of hoge by using pointer hoge_p
    printf("*hoge_p -> %d\n",*hoge_p);

    // Change the value of hoge through hoge_p
    *hoge_p=10;
    printf("hoge -> %d\n",hoge);
 
    return 0;
}


int main(void){
    checkaddress();
}