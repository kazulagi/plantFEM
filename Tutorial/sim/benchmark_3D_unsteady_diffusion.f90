program main
use FEMDomainClass
use DiffusionEquationClass
implicit none

type(DiffusionEq_) :: solver
type(FEMDomain_) :: cube(4)
real(real64),allocatable :: c(:)
real(real64),allocatable :: DiffusionCoeff(:)
real(real64),allocatable :: Reaction(:)
real(real64),allocatable :: FixValue(:)
real(real64),allocatable :: C_n(:)
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: itr,offset
type(Random_) :: random
real(real64) :: Length,epsi
type(IO_) :: f

Length=1.0d0
epsi=0.010d0


! EbO-FEM diffusion

call cube(1)%create("Cube3D",x_num=10,y_num=5,z_num=5)
call cube(2)%create("Cube3D",x_num=10,y_num=5,z_num=5)
call cube(3)%create("Cube3D",x_num=10,y_num=5,z_num=5)
call cube(4)%create("Cube3D",x_num=10,y_num=5,z_num=5)

call cube(1)%resize(x=Length/4+epsi,y=Length/10.0d0,z=Length/10.0d0)
call cube(2)%resize(x=Length/4+epsi,y=Length/10.0d0,z=Length/10.0d0)
call cube(3)%resize(x=Length/4+epsi,y=Length/10.0d0,z=Length/10.0d0)
call cube(4)%resize(x=Length/4+epsi,y=Length/10.0d0,z=Length/10.0d0)

call cube(1)%move(x=0.0d0)
call cube(2)%move(x=cube(1)%xmax()-epsi)
call cube(3)%move(x=cube(2)%xmax()-epsi)
call cube(4)%move(x=cube(3)%xmax()-epsi)

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
    // zeros(size(cube(4)%select(x_min=cube(4)%xmax() )))

DiffusionCoeff &
    = 1.0e-4*eyes( cube(1)%ne()+ cube(2)%ne()+ cube(3)%ne()+cube(4)%ne() )

C_n = cube(1)%full(func=sin_function)
C_n = C_n // cube(2)%full(func=sin_function)
C_n = C_n // cube(3)%full(func=sin_function)
C_n = C_n // cube(4)%full(func=sin_function)


c = c_n
itr=0
call cube(1)%vtk(name="init_1_"+zfill(itr,4),scalar=c(               1 &
: cube(1)%nn() ) )
call cube(2)%vtk(name="init_2_"+zfill(itr,4),scalar=c(cube(1)%nn() + 1 &
: cube(1)%nn() + cube(2)%nn() ) )
call cube(3)%vtk(name="init_3_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + 1 &
: cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() ) )
call cube(4)%vtk(name="init_4_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + cube(3)%nn() + 1 &
: cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() + cube(4)%nn() ) )


do itr=1,100

    Reaction &
        = zeros( cube(1)%ne()+ cube(2)%ne()+ cube(3)%ne()+cube(4)%ne() )
    !Reaction(1000:1100) = random%gauss(mu=3000.0d0,sigma=100.0d0)

    solver%solver%debug=True
    solver%solver%itrmax=100000
    c = solver%getDiffusionField(&
        FEMDomains=cube, &
        DiffusionCoeff=DiffusionCoeff, &
        Reaction=Reaction, &
        Penalty=10000.0d0 ,&
        FixBoundary=FixBoundary,   &
        FixValue=FixValue   ,    &
        C_n=C_n,&
        dt=3.0d0 &
        )
    C_n = c
    !
    call cube(1)%vtk(name="analysis_1_"+zfill(itr,4),scalar=c(               1 &
        : cube(1)%nn() ) )
    call cube(2)%vtk(name="analysis_2_"+zfill(itr,4),scalar=c(cube(1)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn() ) )
    call cube(3)%vtk(name="analysis_3_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() ) )
    call cube(4)%vtk(name="analysis_4_"+zfill(itr,4),scalar=c(cube(1)%nn() + cube(2)%nn() + cube(3)%nn() + 1 &
        : cube(1)%nn() + cube(2)%nn()+ cube(3)%nn() + cube(4)%nn() ) )
enddo

call f%open("result.csv","w")
offset = 0
do i_i=1,cube(1)%nn()
    call f%write( cube(1)%position_x(i_i), c(i_i)  )
enddo
offset = offset + cube(1)%nn()
do i_i=1,cube(2)%nn()
    call f%write( cube(2)%position_x(i_i), c(i_i + offset)  )
enddo
offset = offset + cube(2)%nn()
do i_i=1,cube(3)%nn()
    call f%write( cube(3)%position_x(i_i), c(i_i + offset)  )
enddo
offset = offset + cube(3)%nn()
do i_i=1,cube(4)%nn()
    call f%write( cube(4)%position_x(i_i), c(i_i + offset)  )
enddo
call f%close()


call f%open("analytical.csv","w")
do j_j=1,size(cube)
do i_i=1,cube(j_j)%nn()
    call f%write( cube(j_j)%position_x(i_i), analytical_solution(&   
        x=[cube(j_j)%position_x(i_i),dble(i_i)/dble(cube(j_j)%nn())],&
        params=[DiffusionCoeff(1)]  )  )
enddo
enddo
call f%close()



contains

function sin_function(x,params) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64),optional,intent(in) :: params(:)
    real(real64) :: ret
    type(Math_) :: math

    ret = sin(math%pi*x(1))

end function

function analytical_solution(x,params) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64),optional,intent(in) :: params(:)
    real(real64) :: ret
    type(Math_) :: math

    ! x1 = x
    ! x2 = t
    ret = sin(math%PI*x(1) )&
                    *exp(-params(1)*math%PI*math%PI*x(2) )

end function


end program