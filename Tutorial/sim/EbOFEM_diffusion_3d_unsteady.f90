use DiffusionEquationClass
implicit none

type(DiffusionEq_) :: solver
type(FEMDomain_) :: cube(4)
real(real64),allocatable :: c(:)
real(real64),allocatable :: DiffusionCoeff(:)
real(real64),allocatable :: Reaction(:)
real(real64),allocatable :: FixValue(:)
real(real64),allocatable :: C_n(:)
real(real64),allocatable :: dCdt(:)
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: itr

call cube(1)%create("Cube3D",x_num=20,y_num=5,z_num=5)
call cube(2)%create("Cube3D",x_num=20,y_num=5,z_num=5)
call cube(3)%create("Cube3D",x_num=20,y_num=5,z_num=5)
call cube(4)%create("Cube3D",x_num=20,y_num=5,z_num=5)

call cube(1)%resize(x=20.0d0,y=5.0d0,z=5.0d0)
call cube(2)%resize(x=20.0d0,y=5.0d0,z=5.0d0)
call cube(3)%resize(x=20.0d0,y=5.0d0,z=5.0d0)
call cube(4)%resize(x=20.0d0,y=5.0d0,z=5.0d0)

call cube(1)%move(x=0.0d0)
call cube(2)%move(x=cube(1)%xmax()-1.20d0)
call cube(3)%move(x=cube(2)%xmax()-1.20d0)
call cube(4)%move(x=cube(3)%xmax()-1.20d0)

call cube(1)%vtk("cube_1")
call cube(2)%vtk("cube_2")
call cube(3)%vtk("cube_3")
call cube(4)%vtk("cube_4")

call cube(1)%overset(FEMDomains=cube,to=2,by="GPP")
call cube(2)%overset(FEMDomains=cube,to=3,by="GPP")
call cube(3)%overset(FEMDomains=cube,to=4,by="GPP")

FixBoundary = cube(1)%select(x_max=cube(1)%xmin()) &
    // cube(4)%select(x_min=cube(4)%xmax() ) + cube(1)%nn()+ cube(2)%nn()+ cube(3)%nn()
FixValue    = zeros(size(cube(1)%select(x_max=cube(1)%xmin()) ))&
    // 30.0d0*eyes(size(cube(4)%select(x_min=cube(4)%xmax() )))

C_n   = zeros( cube(1)%nn()+ cube(2)%nn()+ cube(3)%nn()+cube(4)%nn() )
dCdt  = zeros( cube(1)%nn()+ cube(2)%nn()+ cube(3)%nn()+cube(4)%nn() )
DiffusionCoeff &
    = eyes( cube(1)%ne()+ cube(2)%ne()+ cube(3)%ne()+cube(4)%ne() )
Reaction &
    = zeros( cube(1)%ne()+ cube(2)%ne()+ cube(3)%ne()+cube(4)%ne() )

do itr=1,100

    solver%solver%debug=True
    solver%solver%itrmax=100000
    c = solver%getDiffusionField(&
        FEMDomains=cube, &
        DiffusionCoeff=DiffusionCoeff, &
        Reaction=Reaction, &
        Penalty=100.0d0 ,&
        FixBoundary=FixBoundary,   &
        FixValue=FixValue   ,    &
        C_n=C_n,&
        dCdt=dCdt,&
        dt=1.0d0 &
        )
    C_n = c
    !
    call cube(1)%vtk(name="result_1_"+zfill(itr,4),scalar=c(               1 &
        : cube(1)%nn() ) )
    call cube(2)%vtk(name="result_2_"+zfill(itr,4),scalar=c(cube(1)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn() ) )
    call cube(3)%vtk(name="result_3_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() ) )
    call cube(4)%vtk(name="result_4_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + cube(3)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() + cube(4)%nn() ) )
enddo


end