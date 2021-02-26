module STLClass
    use IOClass
    use RandomClass
    use ArrayClass

    type :: STL_
        real(real64),allocatable :: normal(:,:)
        real(real64),allocatable :: Facet(:,:,:)
    contains
        procedure,public :: open => importSTL
        procedure,public :: import => importSTL
        procedure,public :: write => exportSTL
        procedure,public :: export => exportSTL
        procedure,public :: reduceSize => reduceSizeSTL
        procedure,public :: reduce => reduceSizeSTL
    end type

contains

! ######################################################
    subroutine reduceSizeSTL(obj,ratio)
        class(STL_),intent(inout) :: obj
        type(STL_) :: buffer
        real(real64),intent(in) :: ratio ! conpression ratio (0 < ratio < 1)
        type(Random_) :: random
        integer(int32) :: final_num,n, kill_id,i,kill_num,id
        logical,allocatable :: kill_list(:)

        if(ratio > 1.0d0 .or. ratio < 0.0d0)then
            print *, "reduceSizeSTL :: ERROR :: please input conpression ratio (0 < ratio < 1)"
            return
        endif

        final_num = int(dble(size(obj%Facet,1) )*ratio)
        n = size(obj%Facet,1)
        allocate(kill_list(n) )
        kill_list(:) = .false.
        kill_num = n - final_num
        print *, "Reduce number of facet from ",n," to ",final_num," ratio: ",ratio

        allocate(buffer%facet(final_num,3,3) )
        allocate(buffer%normal(final_num,3) )
        buffer%facet(:,:,:) = 0.0d0
        buffer%normal(:,1:2)=0.0d0
        buffer%normal(:,3)=1.0d0
        call random%init()
        i=0
        do 
            if(i==kill_num) exit
            kill_id = int(dble(n) * random%random())
            if(kill_id==0)then
                kill_id=1
            endif
            if(kill_list(kill_id) .eqv. .true.)then
                cycle
            else
                i=i+1
                kill_list(kill_id) = .true.
            endif
        enddo
        id=0
        do i=n,1,-1
            if(kill_list(i) .eqv. .true. )then
                cycle
            else
                id = id + 1
                buffer%facet(id,:,:) = obj%facet(i,:,:)
            endif
        enddo

        deallocate(obj%facet)
        deallocate(obj%normal)

        obj%facet = buffer%facet
        obj%normal = buffer%normal

    end subroutine
! ######################################################
    subroutine importSTL(obj,name)
        class(STL_),intent(inout) :: obj
        character(*),intent(in) :: name
        character(200) :: ch
        character(5) ::facet
        character(6) ::normal
        real(real64) :: x, y, z
        integer(int32) :: numoffacet,i,j,numnorm,n
        type(IO_) :: f
        type(String_) :: string

        if(index(name, ".stl")==0 .and. index(name, ".STL")==0 )then
            print *, "open ",trim(name)//".stl"
            call f%open(trim(name)//".stl")
        else
            call f%open(trim(name))
        endif
        

        if(allocated(obj%normal) ) deallocate(obj%normal)
        if(allocated(obj%facet) ) deallocate(obj%facet)
        numoffacet=0
        do
            if(f%EOF .eqv. .true.) exit
            string = f%readline()
            ch =trim(adjustl(string%all) )

            if( index(ch,"outer")/=0 .and.index(ch,"loop") /=0 )then
                numoffacet=numoffacet+1
                cycle
            endif
        enddo

        allocate(obj%normal(numoffacet,3) )
        allocate(obj%facet(numoffacet,3,3) )


        call f%close()

        if(index(name, ".stl")==0 .and. index(name, ".STL")==0 )then
            call f%open(trim(name)//".stl")
        else
            call f%open(trim(name))
        endif
        n=numoffacet
        numoffacet=0
        numnorm=0
        do

            if(f%EOF .eqv. .true.) exit
            string = f%readline()
            ch =trim(adjustl(string%all) )
            if(index(ch,"facet")/=0 .and. index(ch,"normal")/=0  )then
                read(ch,*) facet, normal, x,y,z
                numnorm=numnorm+1
                obj%normal(numnorm,1)=x
                obj%normal(numnorm,2)=y
                obj%normal(numnorm,3)=z
                cycle
            endif

            if( index(ch,"outer")/=0 .and.index(ch,"loop")/=0  )then
                numoffacet=numoffacet+1
                do i=1,3
                    read(f%fh,*) ch,x,y,z
                    obj%facet(numoffacet,i,1)=x
                    obj%facet(numoffacet,i,2)=y
                    obj%facet(numoffacet,i,3)=z
                enddo
                cycle
                cycle
            endif

!            if(ch(1:8)=="endfacet" .or. ch(1:5)=="solid"  )then
!                if(numnorm==n)then
!                    exit
!                endif
!                numnorm=numnorm+1
!                read(f%fh,*) facet,normal,x,y,z
!                obj%normal(numnorm,1)=x
!                obj%normal(numnorm,2)=y
!                obj%normal(numnorm,3)=z
!            endif
!
!            if( ch(1:10)=="outer" )then
!                numoffacet=numoffacet+1
!                do i=1,3
!                    read(f%fh,*) ch,x,y,z
!                    obj%facet(numoffacet,i,1)=x
!                    obj%facet(numoffacet,i,2)=y
!                    obj%facet(numoffacet,i,3)=z
!                enddo
!                cycle
!            endif
!
!
!            if( ch(1:8)=="endsolid" )then
!                cycle
!            endif

        enddo

        call f%close()
    end subroutine
! #############################################################################

! ######################################################
    subroutine exportSTL(obj,name)
        class(STL_),intent(inout) :: obj
        character(*),intent(in) :: name
        character(200) :: ch
        character(5) ::facet
        character(6) ::normal
        real(real64) :: x, y, z
        integer(int32) :: numoffacet,i,j,numnorm,n
        type(IO_) :: f

        if(index(name, ".stl")==0 .and. index(name, ".STL")==0 )then
            call f%open(trim(name)//".stl")
        else
            call f%open(trim(name))
        endif

        write (f%fh,'(A)' ) "solid "//trim(name)

        do i=1,size(obj%facet,1)
            write (f%fh,'(A, f10.4, f10.4, f10.4)') "facet normal ",real(obj%normal(i,1)),&
                real(obj%normal(i,2)),real(obj%normal(i,3))
            write (f%fh,'(A)') "outer loop "    
            write (f%fh,'(A, f10.4, f10.4, f10.4)') "vertex ", real(obj%facet(i,1,1)),real(obj%facet(i,1,2)),real(obj%facet(i,1,3))
            write (f%fh,'(A, f10.4, f10.4, f10.4)') "vertex ", real(obj%facet(i,2,1)),real(obj%facet(i,2,2)),real(obj%facet(i,2,3))
            write (f%fh,'(A, f10.4, f10.4, f10.4)') "vertex ", real(obj%facet(i,3,1)),real(obj%facet(i,3,2)),real(obj%facet(i,3,3))
            write (f%fh,'(A)') "endloop"
            write (f%fh,'(A)') "endfacet"
        enddo

        write (f%fh,'(A)') "endsolid "//trim(name)
        call f%close()
    end subroutine



end module