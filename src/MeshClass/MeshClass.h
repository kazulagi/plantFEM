#include<iostream>
#include<vector>
#include <string.h>



class Mesh{
    

    public:
    const int PF_SINGLE_CELL = 1;

    double **NodCoord;
    // Connectivity information for FE-mesh
    int **ElemNod;
    // Material IDs for Finite Elements
    int ElemMat[0];

    int MasterID[0];
    int SlaveID[0];
    int NTSMasterFacetID[0];
    double **xi;

    // optional data;
    double **NodCoordInit;
    int BottomElemID;
    int TopElemID;
    int **FacetElemNod;
    int **NextFacets;
    int SurfaceLine2D[0];
    int **SubMeshNodFromTo;
    int **SubMeshElemFromTo;
    int **SubMeshSurfFromTo;



    //for Interfaces
    int GlobalNodID[0];

    char uuid;
    char ElemType;
    public:
        char ErrorMsg;
        char meshtype;
        int surface;

    public:
        Mesh();
        void create(int meshtype);
        void showConnectivity();

};

