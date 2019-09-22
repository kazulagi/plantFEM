program main
    use ArrayOperationClass
    implicit none

    integer,allocatable :: mat(:,:)
    integer  :: i

    allocate( mat(1,1) )
    mat(:,:)=1
    call showArray(mat)
    print *, " "
    do i=2,10
        call insertArray(mat,insert1stColumn=.true.,DefaultValue= i, NextOf = i-1 )
        call insertArray(mat,insert2ndColumn=.true.,DefaultValue= i, NextOf = i-1 )
        call showArray(mat)
        print *, " "
    enddo

    do i=2,10
        call removeArray(mat,remove1stColumn=.true. ,NextOf = 5)
        call removeArray(mat,remove2ndColumn=.true. ,NextOf = 5)
        
        call showArray(mat)
        print *, " "
    enddo

end program