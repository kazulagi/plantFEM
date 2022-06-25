use SoybeanClass
implicit none

type(Soybean_) :: soy

type(Soybean_) :: soy_opt(1:100)
real(real64)   :: score(1:100)
type(FEMDomain_):: point_cloud
integer(int32) :: i

call soy%init("Tutorial/obj/soy.json")
! Edit soybean

print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%getWidth()
print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%A_PointNodeID
print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%B_PointNodeID
print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%C_PointNodeID
print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%D_PointNodeID
print *, soy%stem(soy%searchStem(StemID=1,InterNodeID=3))%I_planeNodeID 

call soy%vtk("soy_after",single_file=.true.)

call soy%resizeStem(&
    StemID=1,& ! main=0, branch=1,2 ...
    InterNodeID=3,& ! 1,2,3...
    Length     =0.20d0 ,&! m , optional
    Width      =0.020d0 &! m, optional 
    )

call soy%rotateStem(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=1,& ! 1,2,3...
    angles = [3.0d0, 40.0d0, 20.0d0] &! [x, y, z] angles
    )

call soy%rotateStem(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=2,& ! 1,2,3...
    angles = [3.0d0, 40.0d0, 20.0d0] &! [x, y, z] angles
    )

call soy%rotateStem(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=3,& ! 1,2,3...
    angles = [3.0d0, 40.0d0, 20.0d0] &! [x, y, z] angles
    )
call soy%vtk("soy_before",single_file=.true.)


end