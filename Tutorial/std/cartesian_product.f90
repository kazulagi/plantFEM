use ArrayClass

implicit none
real(real64) :: vars_1(2),vars_2(3),vars_3(2)

vars_1 = [2.0d0,3.0d0]
vars_2 = [1.0d0,4.0d0, 8.0d0]
vars_3 = [100.0d0,200.0d0]
call print(cartesian_product(cartesian_product(vars_1,vars_2),vars_3) )

end