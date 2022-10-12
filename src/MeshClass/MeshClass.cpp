#include <MeshClass.h>
#include <string.h>

const extern int PF_SINGLE_CELL = 1;

Mesh::Mesh(){
    this->surface = 1;
}

void Mesh::create(int meshtype){
    if(meshtype==PF_SINGLE_CELL){
        this->ElemNod = new int*[1];
        for (int i=0; i<1; i++){
            this->ElemNod[i] = new int[8];
        };
        for (int i=0; i<8; i++){
            this->ElemNod[0][i] = i;
        };
    };
    std::cout << "[ok] created mesh type :: " << meshtype << std::endl;
};

void Mesh::showConnectivity(){
    int arrSize1 = std::extent<decltype(this->ElemNod),0>::value ;
    std::cout << "[ok] Number of element is :: "<< arrSize1 << std::endl;
    int arrSize2 = std::extent<decltype(this->ElemNod),1>::value ;
    std::cout << "[ok] Number of node per element is :: "<< arrSize2 << std::endl;
    /*
    for (int i;i<8;i++){
        std::cout << this->ElemNod[0][i] << std::endl;
    };
    */
};