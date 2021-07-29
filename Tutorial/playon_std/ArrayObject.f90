program main
    use ArrayClass
    implicit none

    type(Array_) :: A, B, C, I, str_array
    real(real64),allocatable :: vector(:),small_vector(:)
    integer(int32),allocatable :: vector_int(:)
    real(real64),allocatable :: matrix(:,:)
    integer(int32),allocatable :: matrix_int(:,:)

    print *, " "
    call A%random(3,3)
    call B%zeros(3,3)
    call I%unit(3,3)

    C = A * B ! A_ij B_jk

    C = A + B ! A_ij + B_ij

    call A%print()
    call B%print()
    call C%print()
    call I%print()

    print *, dot(C, C)

    ! create vectors
    print *, "zeros"
    vector = zeros(5) ! Five 0
    call print(vector)

    ! hstack
    print *, "hstack"
    small_vector = [10.0d0, 20.0d0, 30.0d0, 40.0d0]
    call print( hstack(vector, small_vector) )

    ! np.arange
    print *, "arange"
    vector = arange(5) ! 1, 2, 3 , 4, 5
    call print(vector)
    
    ! np.arange
    print *, "arange : start, stop, step"
    vector = arange(10, 50, 5) ! 1, 2, 3 , 4, 5
    call print(vector)
    
    ! np.arange
    print *, "arange : start, stop, step"
    vector_int = int(arange(10, 50, 5)) ! 1, 2, 3 , 4, 5
    call print(vector_int)

    ! reshape
    print *, "arange : start, stop, step"
    matrix = reshape(vector, 7, 3 )
    call print(matrix)

    ! reshape int
    print *, "arange : start, stop, step"
    matrix_int = reshape(vector_int, 7,  3 )
    call print(matrix_int)

    ! allocate string-array
    allocate(str_array%list(4,4) )
    str_array%list(1,1)%string = "hello"
    str_array%list(1,2)%string = "hello"
    str_array%list(1,3)%string = "hello"
    str_array%list(1,4)%string = "hello"
    str_array%list(1,2)%string = "oka"
    str_array%list(3,1)%string = "hoge"
    str_array%list(1,4)%string = "piyo"
    call str_array%print()


end program