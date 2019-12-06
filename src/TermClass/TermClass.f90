module TermClass
    use, intrinsic :: iso_fortran_env
    implicit none

    type :: Term_
        ! post-processors
        character*70 :: gmsh
        character*70 :: gnuplot
        
        ! Solvers
        character*70 :: GaussJordan
        character*70 :: BiCGSTAB
        character*70 :: Result
        character*200:: FiniteDeform
    contains
        procedure :: init => InitializeTerm
    end type

contains

subroutine InitializeTerm(obj)
    class(Term_),intent(inout)::obj

    obj%gmsh="Gmsh"
    obj%gnuplot="gnuplot"

    obj%GaussJordan="GaussJordan"
    obj%BiCGSTAB="BiCGSTAB"

    obj%Result = "./RESULTS"
    obj%FiniteDeform = "FiniteDeform_"



end subroutine

end module