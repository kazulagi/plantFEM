use std
implicit None

type(random_) :: random
type(IO_) :: f
type(Math_) :: math
real(real64) :: width,length,r,x,y,alpha,lin_curve_ratio,y_
integer(int32) :: cases, i

call f%open("circle_model.txt")
do cases=1,100
    width  = random%random()
    lin_curve_ratio = 0.50d0
    length = 1.0d0
    alpha  = width/2.0d0
    r      = (alpha**2 + (length - alpha)**2)/(2*alpha)
    do i=1,1000
        x = dble(i-1)/1000.0d0
        if(x < width/2.0d0)then
            y = sqrt( alpha**2 - (x-alpha)**2 )
            y_ = x
            y = lin_curve_ratio*y + (1.0d0-lin_curve_ratio)*y_
        else
            y_ = alpha + (-alpha)/(length-alpha)*(x - alpha)
            y = alpha - r + sqrt(r**2 - (x - alpha)**2 )
            y = lin_curve_ratio*y + (1.0d0-lin_curve_ratio)*y_
        endif
        !if(y<0.0d0)cycle
        call f%write(x,y)
    enddo
    do i=1,1000
        x = dble(i-1)/1000.0d0
        
        if(x < width/2.0d0)then
            y = sqrt( alpha**2 - (x-alpha)**2 )
            y_ = x
            y = lin_curve_ratio*y + (1.0d0-lin_curve_ratio)*y_
        else
            y = alpha - r + sqrt(r**2 - (x - alpha)**2 )
            y_ = alpha + (-alpha)/(length-alpha)*(x - alpha)
            y = lin_curve_ratio*y + (1.0d0-lin_curve_ratio)*y_
        endif
        !if(y<0.0d0)cycle
        call f%write(x,-y)
    enddo

    call f%write(" ")
enddo 
call f%close()


end