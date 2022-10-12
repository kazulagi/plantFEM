use SoybeanClass
implicit none

type(Soybean_) :: soy

type(Soybean_) :: soy_opt(1:100)
real(real64)   :: score(1:100)
type(FEMDomain_):: point_cloud
integer(int32) :: i

soy%max_num_leaf_per_petiole = 7
call soy%init("Tutorial/obj/soy.json")

! Edit soybean
call soy%vtk("soy_before",single_file=.true.)

print *, soy%searchStem(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=3 & ! 1,2,3...
    )


print *, soy%searchPetiole(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=2,& ! 1,2,3...
    PetioleID=1 &
    )

print *, soy%searchLeaf(&
    StemID=0,& ! main=0, branch=1,2 ...
    InterNodeID=2,& ! 1,2,3...
    PetioleID=1, &
    leafID = 1 &
    )


call soy%vtk("soy_after",single_file=.true.)

end