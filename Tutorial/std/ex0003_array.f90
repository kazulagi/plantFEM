program main
    use std ! standard package of plantFEM library
    implicit none

    ! This example utilizes Array Class, where
    ! You can handle external files.

    ! How to use:

    ! First, create the instance    
    real(real64),allocatable :: a(:,:)
    real(real64),allocatable :: b(:,:)
    real(real64),allocatable :: ab(:,:)
    real(real64),allocatable :: copyobj(:,:)
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: i

    type(IO_) :: f

    ! #1 create array
    ! ----> allocate array
    allocate(ret(3,3) )
    ! ----> set identity matrix
    ret = identity_matrix(3)
    ! show array on terminal.
    print *, "ret = "
    call showArray(ret)

    ! #2 Array Input-Output
    ! ----> save matrix as "./test.txt" file
    call savetxt(ret,"./","test",".txt")
    ! ----> load a .txt or .csv file.
    a = loadtxt("./","test",".txt")
    b = loadtxt("./","test",".txt")

    ! -----> also, you can do it for multiple arrays as
    call f%open("./"//"arrays"//".txt")
    ! save "a" array
    call saveArray(f%fh,a)
    ! save "b" array
    call saveArray(f%fh,b)
    ! save "ret" array
    call saveArray(f%fh,ret)
    call f%close()

    ! -----> and,
    call f%open("./"//"arrays"//".txt")
    ! load "a" array
    call loadArray(f%fh,a)
    ! load "b" array
    call loadArray(f%fh,b)
    ! load "ret" array
    call loadArray(f%fh,ret)
    call f%close()


    ! -----> It shows the array.
    print *, "a = "
    call showArray(a)

    ! -----> copy array
    a(1,2)=20.0d0
    call copyarray(a,copyobj)
    ! -----> It shows the array.
    print *, "a (original) = "
    call showArray(a)
    print *, "copy = "
    call showArray(copyobj)

    ! Importance Index 7 / 10 : [*******   ]

    
end program 