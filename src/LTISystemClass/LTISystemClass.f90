module LTISystemClass
    use ArrayClass
    use IOClass
    use MathClass
    use RandomClass
    implicit none

    type :: LTISystem_
        real(real64),allocatable :: State(:)
        real(real64),allocatable :: ObservVec(:)
        real(real64),allocatable :: PlantNoise(:)
        real(real64),allocatable :: StateTransition(:,:)
        real(real64),allocatable :: Driving(:,:)
        real(real64),allocatable :: ObservMat(:,:)
        real(real64) :: sigma=0.0d0
    contains
        procedure,public :: init => initLTISystem
        procedure,public :: update => updateLTISystem
    end type
contains    
! ######################################################################################
subroutine initLTISystem(obj,State,StateTransition,Driving,ObservMat)
    class(LTISystem_),intent(inout) :: obj
    real(real64),intent(in) :: State(:)
    real(real64),intent(in) :: StateTransition(:,:)
    real(real64),intent(in) :: Driving(:,:)
    real(real64),intent(in) :: ObservMat(:,:)


    obj%State = State
    obj%StateTransition = StateTransition
    obj%Driving = Driving
    obj%ObservMat = ObservMat


end subroutine
! ######################################################################################


! ######################################################################################
function updateLTISystem(obj) result(y)
    class(LTISystem_),intent(inout) :: obj
    type(Random_) :: random
    real(real64),allocatable :: x(:),y(:),w(:),v(:)
    integer(int32) :: i,n,m,p
    m = size(obj%Driving,2)
    p = size(obj%ObservVec)
    w = zeros(m)
    do i=1,m
        w(i) = random%gauss(mu=0.0d0,sigma=obj%sigma)
    enddo
    v = zeros(p)
    do i=1,m
        v(i) = random%gauss(mu=0.0d0,sigma=obj%sigma)
    enddo

    x = matmul(obj%StateTransition,obj%State) + matmul(obj%Driving,w)
    y = matmul(obj%ObservMat,obj%State) + v(:)

    obj%state = x
    obj%ObservVec = y


end function
! ######################################################################################
end module