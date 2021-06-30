#include <iostream>
#include "FEMDomainClass.h"

int FEMDomain::setElementType(int elementType=0){
    if(elementType>0){
        FEMDomain::elementType=elementType;
    }else if(elementType<0){
        std::cout << "[ERROR] :: FEMDomain.cpp >> Invalid Element Type! ::" << elementType <<"\n";
    }
    return FEMDomain::elementType;
}