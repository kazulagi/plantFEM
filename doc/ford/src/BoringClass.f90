module BoringClass
    use std
    implicit none

    ! a Class for Boring Sampling of Ground
    integer(int32) :: SURFACE_SOIL   = 0
    integer(int32) :: CLAY_SOIL      = 1
    integer(int32) :: CLAY_SILT_SOIL = 2
    integer(int32) :: SAND_SOIL      = 3
    integer(int32) :: SAND_MUDSTONE  = 100

    integer(int32) :: DARK_BROUN   = 0
    integer(int32) :: DARK_GRAY    = 1

    type::Boring_
        character(len=:),allocatable :: Project
        character(len=:),allocatable :: Name
        character(len=:),allocatable :: URL
        real(real64) :: position(1:3)=0.0d0
        real(real64) :: TP ! Ground Level
        real(real64) :: Length
        integer(int32) :: SamplingPoint=1
        real(real64),allocatable :: Elevation(:)
        real(real64),allocatable :: Depth(:)
        real(real64),allocatable :: PTest_Depth(:)
        real(real64),allocatable :: PTest_NValue(:)
        integer(int32),allocatable :: SoilType(:)
        integer(int32),allocatable :: Color(:)
    contains
        procedure,public :: create => createBoring
        procedure,public :: example => exampleBoring
    end type
contains


! ########################################################
subroutine createBoring(obj,SamplingPoint)
    class(Boring_), intent(inout) :: obj
    integer(int32),intent(in) :: SamplingPoint

    obj%SamplingPoint = SamplingPoint
    allocate(obj%Elevation(obj%SamplingPoint))
    allocate(obj%Depth(obj%SamplingPoint))
    allocate(obj%PTest_Depth(obj%SamplingPoint))
    allocate(obj%PTest_NValue(obj%SamplingPoint))
    allocate(obj%SoilType(obj%SamplingPoint))
    allocate(obj%Color(obj%SamplingPoint))
    
end subroutine
! ########################################################

! ########################################################
subroutine exampleBoring(obj)
    class(Boring_), intent(inout) :: obj

    obj%SamplingPoint = 4
    obj%name = "A boring in Saitama Pref. JAPAN"
    obj%URL = "http://www.kankyou.pref.saitama.lg.jp/kankyou/newpdf3/00800075.pdf"
    obj%Elevation   = [60.47d0, 58.72d0, 56.67d0, 53.37d0, 52.14d0]
    obj%Depth       = [  0.0d0, -1.75d0, -3.80d0, -7.10d0, -8.33d0]
    obj%PTest_Depth = [ -1.3d0, -2.30d0, -3.30d0, -4.30d0, -5.30d0, -6.30d0, -7.30d0, -8.30d0]
    obj%PTest_NValue= [  4.0d0, 10.0d0,  9.0d0,3.0d0, 6.0d0, 11.0d0, 214.0d0, 83.0d0]
    obj%SoilType    = [ SURFACE_SOIL,CLAY_SOIL,CLAY_SILT_SOIL,SAND_SOIL,SAND_MUDSTONE  ]
    obj%Color       = [ DARK_BROUN, DARK_BROUN, DARK_GRAY, DARK_GRAY ]

end subroutine
! ########################################################
end module