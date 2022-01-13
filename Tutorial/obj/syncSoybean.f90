use SoybeanClass
implicit None

type(Soybean_),allocatable :: soy(:)
type(MPI_)     :: mpid
integer(int32) :: my_id, i

call mpid%start()

allocate( soy(mpid%petot) )
my_id = mpid%myrank+1

! create 1 soybean for each cpu
call soy(my_id)%create(config="Tutorial/obj/soy.json" )
call soy(my_id)%move(x=dble(my_id - 1) )

! sync
do i=1,mpid%petot
    call soy(i)%sync(from=i-1,mpid=mpid)
enddo

! output
if(my_id == 1)then
    do i=1,mpid%petot
        if(.not. allocated(soy(i)%stem) )then
            print *, "myrank",mpid%myrank,"id",i,"empty"
        else
            call soy(i)%vtk("soy_" + str(i),single_file=.true. )
        endif
    enddo
endif

call mpid%end()
end