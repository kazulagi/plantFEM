program main
    use RangeClass
    use MPIClass
    implicit none

    type(Range_) :: range
    type(Random_):: random
    type(MPI_)   :: mpid
    real(real64) :: data_array(200000,3)
    integer(int32) :: i
    logical :: tf

    call mpid%start()

    call range%init(MaxRange=10000.0d0)
    ! set value
    call range%set(x_min=1.0d0, x_max=2.0d0)
    ! or
    range%x_range = [1.0d0, 2.0d0]

    print *, range%get("x")
    print *, range%get("y")
    print *, range%get("z")
    print *, range%get("t")

    data_array(:,1) = random%randn(size(data_array,1) )
    data_array(:,2) = random%randn(size(data_array,1) )
    data_array(:,3) = random%randn(size(data_array,1) )

    !$OMP parallel do private(i)
    !do concurrent(i=1:size(data_array,1))
    do i=1,size(data_array,1)
        do i_i=1,100
            tf =  range%inside(point=data_array(i,1:3) )
        enddo
    enddo
    !$OMP end parallel do

    call mpid%end()

end program main