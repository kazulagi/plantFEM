module TermClass
    use, intrinsic :: iso_fortran_env
    implicit none

    type :: Term_
        ! post-processors

        character*4 :: gmsh="Gmsh"
        character*7 :: gnuplot="gnuplot"
        
        ! Solvers
        character*8  :: BiCGSTAB="BiCGSTAB"
        character*11 :: GaussJordan="GaussJordan"
        character*12 :: Diffusioneq= "Diffusioneq_"
        character*13 :: FiniteDeform= "FiniteDeform_"

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

    obj%Diffusioneq= "Diffusioneq_"
    obj%FiniteDeform = "FiniteDeform_"



end subroutine

end module