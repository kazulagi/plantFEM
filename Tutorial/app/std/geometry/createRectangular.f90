program main
    use std
    implicit none

    type(Rectangle_) :: obj1, obj2

    call obj1%create()
    call obj2%create()

    call obj2%move(x=2.0d0,y=1.90d0,z=2.0d0)

    print *, obj1%contact(obj2)
    

end program main