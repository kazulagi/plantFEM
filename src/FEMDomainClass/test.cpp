#include "iostream"
#include "FEMDomainClass.h"

int main(){
    FEMDomain seed;
    std::cout << seed.setElementType(VTK_VERTEX);
    std::cout << seed.setElementType(-1000);
}
