module STLClass
    use IOClass

    type :: STL_
        real(real64),allocatable :: normal(:,:)
        real(real64),allocatable :: Facet(:,:,:)
    contains
        procedure,public :: import => importSTL
    end type

contains

    subroutine importSTL(obj,FileName)
        class(STL_),intent(inout) :: obj
        character(*),intent(in) :: FileName
        character(200) :: ch
        character(5) ::facet
        character(6) ::normal
        real(real64) :: x, y, z
        integer(int32) :: numoffacet,i,j,numnorm,n
        type(IO_) :: f

        call f%open(trim(FileName))

        if(allocated(obj%normal) ) deallocate(obj%normal)
        if(allocated(obj%facet) ) deallocate(obj%facet)
        numoffacet=0
        do
            read (f%fh,*) ch
            ch =adjustl(ch) 

            if( ch(1:10)=="outer" )then
                numoffacet=numoffacet+1
                cycle
            endif

            if( ch(1:8)=="endsolid" )then
                exit
            endif
        enddo

        allocate(obj%normal(numoffacet,3) )
        allocate(obj%facet(numoffacet,3,3) )

        call f%close()

        call f%open(trim(FileName))
        n=numoffacet
        numoffacet=0
        numnorm=0
        do
            read (f%fh,*) ch
            ch =adjustl(ch) 

            if(ch(1:8)=="endfacet" .or. ch(1:5)=="solid"  )then
                if(numnorm==n)then
                    exit
                endif
                numnorm=numnorm+1
                read(f%fh,*) facet,normal,x,y,z
                obj%normal(numnorm,1)=x
                obj%normal(numnorm,2)=y
                obj%normal(numnorm,3)=z
            endif

            if( ch(1:10)=="outer" )then
                numoffacet=numoffacet+1
                do i=1,3
                    read(f%fh,*) ch,x,y,z
                    obj%facet(numoffacet,i,1)=x
                    obj%facet(numoffacet,i,2)=y
                    obj%facet(numoffacet,i,3)=z
                enddo
                cycle
            endif


            if( ch(1:8)=="endsolid" )then
                exit
            endif

        enddo

        call f%close()
    end subroutine

end module