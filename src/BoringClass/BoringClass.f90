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
        integer(int32) :: SamplingPoint=0
        real(real64),allocatable :: Elevation(:)
        real(real64) :: x=0.0d0
        real(real64) :: y=0.0d0
        real(real64),allocatable :: Depth(:)
        real(real64),allocatable :: PTest_Depth(:)
        real(real64),allocatable :: PTest_NValue(:)
        integer(int32),allocatable :: SoilType(:)
        integer(int32),allocatable :: Color(:)
    contains
        procedure,public :: create => createBoring
        procedure,public :: example => exampleBoring
        procedure,public :: setN => setNBoring
        procedure,public :: getN => getNBoring
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

pure function getNBoring(obj,depth,elevation) result(Nval)
    class(Boring_),intent(in) :: obj
    real(real64),optional,intent(in) :: depth,elevation
    real(real64) :: Nval,offset,dpt
    integer(int32) :: i,n,layer_id

    if(.not.allocated(obj%PTest_NValue) )then
        ! not initialized
        Nval = 0.0d0
        return
    else
        
        if(present(depth))then
            Nval = 0.0d0
            if( depth > maxval(obj%PTest_Depth ))then
                Nval = obj%PTest_NValue(1)
                return
            endif
            do i=1,size(obj%PTest_Depth) 
                layer_id = i
                
                Nval = obj%PTest_NValue(layer_id)

                if(depth > obj%PTest_Depth(i) )then
                    return
                endif
            enddo
        endif


        if(present(elevation))then
            offset = maxval(obj%Elevation)
            dpt = elevation - offset 
            
            Nval = 0.0d0

            if( dpt > maxval(obj%PTest_Depth ))then
                Nval = obj%PTest_NValue(1)
                return
            endif
            
            do i=1,size(obj%PTest_Depth) 
                layer_id = i

                Nval = obj%PTest_NValue(layer_id)
                
                if(dpt > obj%PTest_Depth(i) )then
                    return
                endif
            enddo
        endif
    endif

end function
! #####################################################


! #####################################################
subroutine setNBoring(obj,depth,Nvalue)
    class(Boring_) ,intent(inout) :: obj
    real(real64),intent(in) :: depth, Nvalue
    real(real64),allocatable :: PTest_Depth(:), PTest_NValue(:)
    integer(int32) :: i,id
    logical :: inserted=.false.

    ! set N-value
    if(.not.allocated(obj%PTest_Depth) )then
        if(allocated(obj%PTest_NValue) )then
            deallocate(obj%PTest_NValue)
        endif
        ! first commit
        obj%PTest_Depth  = zeros(1)
        obj%PTest_NValue = zeros(1)
        obj%PTest_Depth  = depth
        obj%PTest_NValue = Nvalue
        return
    else
        PTest_Depth = zeros(size(obj%PTest_Depth) + 1 )
        PTest_NValue = zeros(size(obj%PTest_NValue) + 1 )
        id = 0
        if(minval(obj%PTest_Depth)>depth )then
            id =size(obj%PTest_Depth)
            PTest_Depth( 1:id)  = obj%PTest_Depth( 1:id)
            PTest_NValue(1:id)  = obj%PTest_NValue(1:id)
            PTest_Depth( id+1)  = depth
            PTest_NValue(id+1)  = Nvalue

            obj%PTest_Depth = PTest_Depth
            obj%PTest_NValue = PTest_NValue

            return
        endif
        id = 0
        inserted = .false.
        do i=1,size(obj%PTest_Depth)
            if( obj%PTest_Depth(i) < depth .and. .not.inserted )then
                id=id+1
                PTest_Depth(id) = depth
                PTest_NValue(id) = Nvalue
                inserted = .true.
            endif
            if( obj%PTest_Depth(i) == depth )then
                obj%PTest_Depth(i) = depth
                obj%PTest_NValue(i) = Nvalue
                return
            endif
            
            id = id + 1
            PTest_Depth(id)  = obj%PTest_Depth(i)
            PTest_NValue(id) = obj%PTest_NValue(i)
        enddo

        obj%PTest_Depth = PTest_Depth
        obj%PTest_NValue = PTest_NValue
        
    endif

end subroutine
! #####################################################


end module