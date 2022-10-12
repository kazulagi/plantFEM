program main

use SoybeanClass
implicit none

type(Soybean_) :: soy

type(Soybean_) :: soy_opt(1:100)
real(real64)   :: score(1:100)
type(FEMDomain_):: point_cloud
integer(int32) :: i, time_step
integer(int32) :: StemID,InterNodeID,PetioleID,LeafID
real(real64) :: t, L,R,Lmax,L0,phi ! phase delay

soy%max_num_leaf_per_petiole = 3
call soy%init("Tutorial/obj/mini_soy.json")

! Edit soybean
call soy%vtk("soy_before",single_file=.true.)
call soy%grow(dt = 100.0d0,simple=.true.)

do time_step = 1, 200
    print *,"step", time_step,"nodes",soy%NodeID_MainStem
    call soy%grow(dt = 1.0d0,simple=.true.,add_apical=.true.)
    call soy%vtk("soy_"+zfill(time_step,4) ,single_file=.true.)
enddo

call soy%vtk("soy_after",single_file=.true.)

end program