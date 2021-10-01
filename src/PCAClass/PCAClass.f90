module PCAClass
    use IOClass
    use MathClass
    use ArrayClass
    implicit none

    type :: PCA_
        logical :: columns_are_feature_names=.true.
        real(real64),allocatable :: DataFrame(:,:)
        real(real64),allocatable :: CovMatrix(:,:)
        real(real64),allocatable :: EigenVector(:,:)
        real(real64),allocatable :: EigenValue(:)
    contains
        procedure :: load => loadPCA
        procedure :: standarize => standarizePCA
        procedure :: run => runPCA
        procedure :: principalComponent =>principalComponentPCA
    end type
contains

! #############################################################
subroutine loadPCA(obj,filename,DataFrame,columns_are_feature_names,num_feature) 
    class(PCA_),intent(inout) :: obj
    real(real64),optional,intent(in) :: DataFrame(:,:)
    character(*),optional,intent(in) :: filename
    logical,optional,intent(in) :: columns_are_feature_names
    integer(int32),optional,intent(in) :: num_feature
    type(IO_) ::f 
    character(:),allocatable :: line
    integer(int32) :: n

    if(present(filename) )then
        if(.not.present(num_feature) )then
            print *, "ERROR :: LoadPCA >> Argument: num_feature should be set."
            stop
        endif
        n = f%numLine(filename)
        obj%DataFrame = zeros(n,num_feature)
        call f%open(filename,"r")
        n=1
        do while(n<=10)
            line = f%readline()
            print *, line
            read(line,*) obj%DataFrame(n,1:num_feature)
            n=n+1
        enddo
        call f%close()
        call print(obj%DataFrame)
    else
        if(present(columns_are_feature_names) )then
            obj%columns_are_feature_names = columns_are_feature_names
        endif
        obj%DataFrame = DataFrame    
    endif

end subroutine
! #############################################################


! #############################################################
subroutine standarizePCA(obj)
    class(PCA_),intent(inout) :: obj
    real(real64) :: sdval, aveval
    integer(int32) :: n

    ! get covarianceMatrix
    if(.not.obj%columns_are_feature_names)then
        n=size(obj%DataFrame,1)
        obj%DataFrame = transpose(obj%DataFrame)
    endif

    do n=1,size(obj%DataFrame,2)
        sdval = standardDeviation(obj%DataFrame(:,n) )
        aveval = average( obj%DataFrame(:,n) )
        obj%DataFrame(:,n) =obj%DataFrame(:,n)-aveval
        obj%DataFrame(:,n) =obj%DataFrame(:,n)/sdval
    enddo

end subroutine
! #############################################################

! #############################################################
subroutine runPCA(obj)
    class(PCA_),intent(inout) :: obj
    integer(int32) :: i,j,n


    ! get covarianceMatrix
    if(obj%columns_are_feature_names)then
        n=size(obj%DataFrame,2)
        obj%CovMatrix = covarianceMatrix(obj%DataFrame,obj%DataFrame,n)
    else
        n=size(obj%DataFrame,1)
        obj%DataFrame = transpose(obj%DataFrame)
        obj%columns_are_feature_names = .true.
        obj%CovMatrix = covarianceMatrix(obj%DataFrame,&
            obj%DataFrame,n)
    endif


    call eigenValueAndVector(obj%CovMatrix,lambda=obj%EigenValue,x=obj%eigenVector)

    print *, "Eigen value::"
    call print(obj%EigenValue)
    do i=1,size(obj%EigenValue)
        print *, "Eigen Vector #"//str(i)
        print *, obj%eigenVector(:,i)
    enddo

end subroutine
! #############################################################

! #############################################################
function principalComponentPCA(obj) result(ret)
    class(PCA_),intent(in) :: obj
    real(real64),allocatable :: ret(:,:),Wmat(:,:)
    integer(int32) :: numdata, numfeature

    numdata   =size(obj%DataFrame,1)
    numfeature=size(obj%DataFrame,2)
    ret = zeros(numdata,2)
    Wmat = zeros(numfeature,2)
    Wmat(:,1) = obj%eigenVector(1,:)
    Wmat(:,2) = obj%eigenVector(2,:)

    ! First Principal Component
    ret(:,1:2) = matmul(obj%DataFrame,Wmat)


end function
! #############################################################

end module PCAClass