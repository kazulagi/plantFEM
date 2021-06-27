#include <stdio.h>
#include <string>

const int VTK_VERTEX		 = 1 ; //	Vertex
const int VTK_POLY_VERTEX = 2 ; //	Vertex
const int VTK_LINE		 = 3 ; //	Edge Lagrange P1
const int VTK_TRIANGLE	 = 5 ; //	Triangle Lagrange P1
const int VTK_PIXEL		 = 8 ; //	Quadrilateral Lagrange P1
const int VTK_QUAD		 = 9 ; //	Quadrilateral Lagrange P1
const int VTK_TETRA		 = 10 ; //	Tetrahedron Lagrange P1
const int VTK_VOXEL		 = 11 ; //	Hexahedron Lagrange P1
const int VTK_HEXAHEDRON  = 12 ; //	Hexahedron Lagrange P1
const int VTK_WEDGE		 = 13 ; //	Wedge Lagrange P1
const int VTK_QUADRATIC_EDGE 	 = 21 ; //	Edge Lagrange P2
const int VTK_QUADRATIC_TRIANGLE  = 22 ; //	Triangle Lagrange P2
const int VTK_QUADRATIC_QUAD		 = 23 ; //	Quadrilateral Lagrange P2
const int VTK_QUADRATIC_TETRA	 = 24 ; //	Tetrahedron Lagrange P2
const int VTK_QUADRATIC_HEXAHEDRON = 25 ; //	Hexahedron Lagrange 
const int MSH_LINE		 = 1  ;//Edge Lagrange P1
const int MSH_TRIANGLE	 = 2  ;//Triangle Lagrange P1
const int MSH_QUAD		 = 3  ;//Quadrilateral Lagrange P1
const int MSH_TETRA		 = 4  ;//Tetrahedron Lagrange P1
const int MSH_HEXAHEDRON  = 5  ;//Hexahedron Lagrange P1
const int MSH_PRISM 	 = 6  ;//Edge Lagrange P2
const int MSH_PYRAMID  = 7  ;//Triangle Lagrange P2

class FEMDomain{
    public :
        double realTime = 1.0;
        int    numOfDomain = 1;
        int    DomainID=1;
        int    timestep=1;
        int    NumberOfBoundaries=0;
        int    NumberOfMaterials=0;
        int    elementType=0;
        std::string filePath = "None";
        std::string fileName = "None";
        std::string name = "None";
        std::string dType = "None";
        std::string solverType = "None";
        std::string category1 = "None";
        std::string category2 = "None";
        std::string category3 = "None";

        FEMDomain(bool dflt=false, bool simple=false, std::string fileName="NoName"){
    
            FEMDomain::fileName = fileName;

            if(simple){
                return;
            }

            if(dflt){
                FEMDomain::dType = "FEMDomain";
            }

        };
        int setElementType(int elementType);
};
