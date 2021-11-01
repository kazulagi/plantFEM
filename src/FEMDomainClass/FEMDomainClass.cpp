#include <iostream>
#include <string>
#include "FEMDomainClass.h"

int FEMDomain::setElementType(int elementType=0){
    if(elementType>0){
        FEMDomain::elementType=elementType;
    }else if(elementType<0){
        std::cout << "[ERROR] :: FEMDomain.cpp >> Invalid Element Type! ::" << elementType <<"\n";
    }
    return FEMDomain::elementType;
};

void FEMDomain::create(std::string MeshType="None",int x_num, int y_num,int z_num, 
    float x_len, float y_len, float z_len){

    /*
    \\ type(FEMDomain_),optional,intent(inout) :: master,slave
	[] character(*),intent(in) :: meshtype
	[] character(*),optional,intent(in) ::Name
	[] integer(int32),optional,intent(in) :: x_num,y_num,z_num ! number of division
	integer(int32) :: xnum,ynum,znum ! number of division
    [] integer(int32),optional,intent(in) :: division ! for 3D rectangular
	[] real(real64),optional,intent(in) :: x_len,y_len,z_len,Le,Lh,Dr ! length
	real(real64) :: xlen,ylen,zlen ! length
	[] real(real64),optional,intent(in) :: thickness ! for 3D rectangular
	[] real(real64),optional,intent(in) :: shaperatio ! for 3D leaf
    [] real(real64),optional,intent(in) :: top,margin,inclineRate ! for 3D Ridge and dam
	[] real(real64),optional,intent(in) :: x,y,z,dx,dy,dz,coordinate(:,:)
	[] integer(int32),optional,intent(in) :: species
	[] real(real64),optional,intent(in) :: SoyWidthRatio
	integer,dimension(3),parameter :: versions_to_test = [0,1,4]
    */

    std::cout << MeshType << std::endl;
    

}