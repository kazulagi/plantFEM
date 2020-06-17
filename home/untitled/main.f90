program main
    use obj
    implicit none

    !type(Ridge_)    :: a
    !call a%create(x_num=10,y_num=10,z_num=10,x_len=120.0d0,y_len=200.0d0,z_len=30.0d0,top=40.0d0)
    !call a%show()

    type(Dam_)    :: a
    call a%create(x_num=10,y_num=10,z_num=10,x_len=120.0d0,y_len=200.0d0,z_len=30.0d0,top=40.0d0)
    call a%show()
end program main