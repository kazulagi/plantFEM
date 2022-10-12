#include "iostream"
#include "FEMDomainClass.h"

int main(){
    FEMDomain seed;

    seed.create("Cube3D");
    std::cout << seed.setElementType(VTK_VERTEX);
}
