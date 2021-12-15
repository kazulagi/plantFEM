! 役割がDictionaryClassと類似しているので，
! 保留 @ 2021.12.10

module ListClass
    use iso_fortran_env
    implicit none

    type :: List_content_
        real(real64) :: Real64Scalar
        real(real64),allocatable :: Real64Vector(:)
        real(real64),allocatable :: Real64Tensor(:,:)
        
        integer(int32) :: Int32Scalar
        integer(int32),allocatable :: Int32Vector(:)
        integer(int32),allocatable :: Int32Tensor(:,:)
    end type

    type :: List_ 
        type(List_content_),allocatable :: content(:)
    contains
        procedure,public :: init => initList
    end type
contains

! ######################################################
subroutine initList(this,maxContent)
    class(List_),intent(inout) :: this
    integer(int32),intent(in) :: maxContent

    
end subroutine
! ######################################################


end module ListClass