module NewModule
    use iso_fortran_env
    use ArrayClass
    use RandomClass
    use IOClass
    implicit none

contains



end module

program main
    use LinearSolverClass
    implicit none
    
    integer(int32) :: i
    real(real64),allocatable :: coeff(:),fit_coeff(:)
    real(real64),allocatable :: training_data(:,:)
    real(real64) :: err
    type(IO_) :: f

    training_data = zeros(21,2)
    ! input
    training_data(:,1) = linspace([-10.0d0,10.0d0],21)
    ! result
    training_data(:,2) = training_data(:,1)*training_data(:,1)*training_data(:,1) - training_data(:,1) +1.0d0

    ! trial : -x^3 + x^2 + x + 1
    coeff = [-1.0d0, 1.0d0, 1.0d0, 1.0d0]

    ! fitting by Robbin-Monro algorithm (SGD)
    fit_coeff = fit(f=polynomial, training_data=training_data, params=coeff ,eta=0.000001d0, error=err,logfile="SGD.log")

    ! show result
    call f%open("training_data.txt","w")
    do i=1,size(training_data,1)
        call f%write(training_data(i,1), training_data(i,2) )
    enddo
    call f%close()

    call f%open("SGD_fit.txt","w")
    do i=-20,20
        call f%write(dble(i), polynomial(x=dble(i),params=fit_coeff ) )
    enddo
    call f%close()
    call f%plot()

end program main

