use LeafClass
implicit none

type(Leafset_) :: leafset(1:3)
real(real64),allocatable :: params(:)

! growth parameters of leafset
real(real64),allocatable :: K_pL ! maximum size of each petiole internodes (m)
real(real64),allocatable :: K_pR ! maximum size of each petiole radius (m)
real(real64),allocatable :: K_lL ! maximum size of each leaf  (m)
real(real64),allocatable :: T_pL ! Delay time (time constant) of each petiole internodes (s)
real(real64),allocatable :: T_pR ! Delay time (time constant) of each petiole radius (s)
real(real64),allocatable :: T_lL ! Delay time (time constant) of each leaf internodes (s

K_pL = 1.0d0
K_pR = 0.010d0
K_lL = 3.0d0

T_pL = 60.0d0*60.0d0*24.0d0*3.0d0
T_pR = 60.0d0*60.0d0*24.0d0*1.50d0
T_lL = 60.0d0*60.0d0*24.0d0*6.0d0

params = [K_pL , K_pR , K_lL , T_pL , T_pR , T_lL]
call leafset(1)%init(num_leaf=7,params=params,species=PF_GLYCINE_SOJA,dt=60.0d0*60.0d0*20.0d0,direction=radian(0.0d0))
call leafset(2)%init(num_leaf=7,params=params,species=PF_GLYCINE_SOJA,dt=60.0d0*60.0d0*20.0d0,direction=radian(120.0d0))
call leafset(3)%init(num_leaf=7,params=params,species=PF_GLYCINE_SOJA,dt=60.0d0*60.0d0*20.0d0,direction=radian(240.0d0))

call leafset(1)%vtk("leafset_1_"+zfill(0,4),single_file=.true.)
call leafset(2)%vtk("leafset_2_"+zfill(0,4),single_file=.true.)
call leafset(3)%vtk("leafset_3_"+zfill(0,4),single_file=.true.)
print *, degrees(leafset(1)%rot_z), degrees(leafset(2)%rot_z), degrees(leafset(3)%rot_z)


do i_i=1,30
    call leafset(1)%grow_peti_and_leaf(params=params,dt=60.0d0*60.0d0*3.0d0)
    call leafset(2)%grow_peti_and_leaf(params=params,dt=60.0d0*60.0d0*3.0d0)
    call leafset(3)%grow_peti_and_leaf(params=params,dt=60.0d0*60.0d0*3.0d0)

    call leafset(1)%vtk("leafset_1_"+zfill(i_i,4),single_file=.true.)
    call leafset(2)%vtk("leafset_2_"+zfill(i_i,4),single_file=.true.)
    call leafset(3)%vtk("leafset_3_"+zfill(i_i,4),single_file=.true.)
enddo

end