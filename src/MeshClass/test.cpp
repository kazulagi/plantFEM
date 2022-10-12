#include <iostream>
#include <MeshClass.h>

int main(){
    class Mesh test;

    test.create(test.PF_SINGLE_CELL);
    
    test.showConnectivity();
};

