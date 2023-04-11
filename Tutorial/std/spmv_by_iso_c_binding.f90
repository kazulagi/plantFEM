program real_array_example
    use SparseClass
    use TimeClass
    use RandomClass
    use FEMDomainClass
    Implicit None


    real(C_float)  ,allocatable :: val(:)
    integer(C_int64_t) ,allocatable :: row_ptr(:)
    integer(C_int) ,allocatable :: col_idx(:)
    !real(C_float) ,allocatable  :: old_vector(:)
    !real(C_float) ,allocatable  :: new_vector(:)
    real(real64) ,allocatable  :: old_vector(:)
    real(real32) ,allocatable  :: old_vector_real32(:)
    real(real64) ,allocatable  :: new_vector(:)
    real(real32) ,allocatable  :: new_vector_real32(:)
    integer(C_size_t) n,col_size
    type(CRS_) :: crs
    type(COO_) :: coo
    type(time_) :: time
    type(Random_) :: random
    type(FEMDomain_) :: domain

    !call coo%poisson(600000)
    !crs = coo%to_crs()

    call domain%create("Cube3D",x_num=100,y_num=100,z_num=10)
    crs = domain%StiffnessMatrix(&
        YoungModulus=1000.0d0*ones(domain%ne()),&
        PoissonRatio=0.30d0*ones(domain%ne()) )
    !
    val     = crs%val
    row_ptr = crs%row_ptr
    col_idx = crs%col_idx
    old_vector = random%randn(crs%size() )
    col_size = size(col_idx)
    n = size(new_vector)
    
    new_vector(:) = 0.0d0
    call time%start()
    do i_i=1,3000
        new_vector = crs%matmul(old_vector,c_routine="true")
    enddo
    call time%show()
    print *, norm(new_vector) 

    new_vector(:) = 0.0d0
    call time%start()
    do i_i=1,3000
        new_vector = crs%matmul(dble(old_vector))
    enddo
    call time%show()
    print *, norm(new_vector) 
    


end program