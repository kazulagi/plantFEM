use std
implicit none

type(Console_) :: console
integer(int32) :: int_num
real(real64) :: real_num


! Interactive application

int_num = 0
real_num = 0.0d0
do while( .not. console%in("exit") )

    call console%log(">>>")

    call console%read() 

    if( console%in("password") )then
        call console%log("My password is :: hello")
        cycle
    endif

    if( console%in("add") )then
        call console%read()
        int_num = int_num + console%asInt()
        call print(int_num)
        cycle
    endif

    if( console%in("+") )then
        call console%read()
        real_num = real_num + console%asReal()
        call print(real_num)
        cycle
    endif


    if( console%in("-") )then
        call console%read()
        real_num = real_num - console%asReal()
        call print(real_num)
        cycle
    endif

    if( console%in("*") )then
        call console%read()
        real_num = real_num * console%asReal()
        call print(real_num)
        cycle
    endif
    

    if( console%in("\") )then
        call console%read()
        real_num = real_num/console%asReal()
        call print(real_num)
        cycle
    endif
enddo


end