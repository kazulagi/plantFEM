program main
    use RandomClass
    use IOClass
    implicit none

    type(random_) :: random
    real(real64) :: x(1000),y(1000),z(1000)

    call random%fill(vector=x)
    call random%fill(vector=y)
    call random%fill(vector=z)

    call plot(x=x,y=y,z=z,xr="[0.1:0.5]",yr="[0.2:0.5]",zr="[0.1:0.9]")
    
end program