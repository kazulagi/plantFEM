program main
   use MaterialPropClass
   implicit none

   type(MaterialProp_)::Mat
   real(8)::mat_cons(2, 6)

   mat_cons(:, 1) = 10000.0d0
   mat_cons(:, 2) = 0.35d0
   mat_cons(:, 3) = 0.0d0
   mat_cons(:, 4) = 10000.0
   mat_cons(:, 5) = 2.0d0
   mat_cons(:, 6) = 0.5d0

   mat_cons(1, 1) = 10.0d0
   call ImportMatPara(Mat, mat_cons)
   call ShowMatPara(Mat)

end program main
