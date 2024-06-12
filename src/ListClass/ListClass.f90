module ListClass
    use iso_fortran_env
    implicit none

    type :: List_content_
        character(:),allocatable :: char
    end type

    type :: List_ 
        type(List_content_),allocatable :: content(:)
    contains
        procedure,public :: get => get_list_content_listclass
        procedure,public :: append => append_list_content_listclass
        procedure,public :: new => new_list_listclass
        procedure,public :: print => print_listclass
        procedure,public :: size => size_listclass

        procedure,public :: help => help_listclass
        procedure,public :: split => split_char_into_list        
        
        
    end type

    interface to_list    
        module procedure to_list_repeat_listclass,to_list_0_listclass,to_list_1_listclass,&
            to_list_2_listclass,to_list_3_listclass,to_list_4_listclass,&
            to_list_5_listclass,to_list_6_listclass,to_list_7_listclass,&
            to_list_int32vec_listclass,&
            to_list_real32vec_listclass,& 
            to_list_real64vec_listclass

    end interface

    

    interface operator(//)
        module procedure joint_listclass
    end interface

    interface operator(//)
        module procedure joint_listcontentclass,joint_arraylistcontentclass
    end interface


contains
! #####################################################
pure function get_list_content_listclass(this,idx) result(ret)
    class(List_),intent(in) :: this
    integeR(int32),intent(in) :: idx
    character(:),allocatable :: ret
    
    if(.not.allocated(this%content) )then
        ret = ""
        return
    endif

    if(.not.allocated(this%content(idx)%char) )then
        ret  = ""
    else
        ret = this%content(idx)%char
    endif

end function
! #####################################################

pure subroutine new_list_listclass(this,length)
    class(List_),intent(inout) :: this
    integer(int32),intent(in) :: length
    integer(int32) :: i

    if(allocated(this%content) )then
        deallocate(this%content)
    endif

    allocate(this%content(length))
    do i=1,length
        this%content(i)%char = ""
    enddo


end subroutine

! #####################################################
subroutine append_list_content_listclass(this,char) 
    class(List_),intent(inout) :: this
    character(*),intent(in) :: char
    type(List_) :: buf
    integer(int32) :: i,n

    buf = this
    n = size(buf%content)
    call this%new(length=size(buf%content)+1 )
    
    do i=1,size(buf%content)
        this%content(i)%char = buf%content(i)%char
    enddo
    
    this%content( n+1 )%char = char

end subroutine
! #####################################################

! #####################################################
pure function size_listclass(this) result(ret)
    class(List_),intent(in) :: this
    integer(int32) :: ret

    ret = size(this%content)

end function
! #####################################################



! #####################################################
subroutine print_listclass(this) 
    class(List_),intent(in) :: this
    integer(int32) :: i

    do i=1,this%size()
        print *, this%content(i)%char 
    enddo

end subroutine
! #####################################################
pure function joint_listcontentclass(List_content_1,List_content_2) result(List_content)
    type(List_content_),intent(in) :: List_content_1,List_content_2
    type(List_content_) :: List_content

    List_content%char = List_content_1%char  // List_content_2%char 

end function


! #####################################################
pure function joint_listclass(list1,list2) result(ret_list)
    type(List_),intent(in) :: list1, list2
    type(List_) :: ret_list
    integer(int32) :: i

    if(allocated(list1%content) .and. .not.allocated(list2%content) )then
        ret_list = list1
        return
    endif

    if(allocated(list2%content) .and. .not.allocated(list1%content) )then
        ret_list = list2
        return
    endif


    if(.not.allocated(list1%content) .and. .not.allocated(list2%content) )then
        return
    endif

    call ret_list%new( list1%size() + list2%size() )

    do i=1,list1%size()
        ret_list%content(i)%char = list1%content(i)%char
    enddo
    do i=1,list2%size()
        ret_list%content(i+list1%size())%char = list2%content(i)%char
    enddo
    

end function




pure function joint_arraylistcontentclass(List_content_1,List_content_2) result(List_content)
    type(List_content_),intent(in) :: List_content_1(:),List_content_2(:)
    type(List_content_),allocatable :: List_content(:)
    integer(int32) :: i

    List_content = List_content_1

    do i=1,size(List_content_1)
        List_content(i)%char = List_content_1(i)%char  // List_content_2(i)%char 
    enddo

end function









! #####################################################
function to_list_0_listclass() result(this)
    type(List_) :: this

    allocate(this%content(0) )

end function
! #####################################################

function to_list_repeat_listclass(char1,num_repeat) result(this)
    character(*),intent(in) :: char1
    type(List_) :: this
    integer(int32),intent(in) :: num_repeat
    integer(int32) :: i

    allocate(this%content(num_repeat) )
    do i=1,num_repeat
        this%content(i)%char = char1
    enddo

end function

! #####################################################


! #####################################################

function to_list_1_listclass(char1) result(this)
    character(*),intent(in) :: char1
    character(:),allocatable :: buf
    type(List_) :: this
    integer(int32) :: ac_from,ac_to


    ac_from = index(char1,"[")
    ac_to   = index(char1,"]",back=.true.)
    if( ac_from/=0 .and. ac_to /=0 )then
        buf = char1(ac_from+1:ac_to-1)
        call this%split(buf,",")
    else
        allocate(this%content(1) )
        this%content(1)%char = char1
    endif
end function

! #####################################################

function to_list_2_listclass(char1,char2) result(this)
    character(*),intent(in) :: char1,char2
    type(List_) :: this

    allocate(this%content(2) )
    this%content(1)%char = char1
    this%content(2)%char = char2

end function


! #####################################################

function to_list_3_listclass(char1,char2,char3) result(this)
    character(*),intent(in) :: char1,char2,char3
    type(List_) :: this

    allocate(this%content(3) )
    this%content(1)%char = char1
    this%content(2)%char = char2
    this%content(3)%char = char3

end function

! #####################################################

function to_list_4_listclass(char1,char2,char3,char4) result(this)
    character(*),intent(in) :: char1,char2,char3,char4
    type(List_) :: this

    allocate(this%content(4) )
    this%content(1)%char = char1
    this%content(2)%char = char2
    this%content(3)%char = char3
    this%content(4)%char = char4

end function

! #####################################################


! #####################################################

function to_list_5_listclass(char1,char2,char3,char4,char5) result(this)
    character(*),intent(in) :: char1,char2,char3,char4,char5
    type(List_) :: this

    allocate(this%content(5) )
    this%content(1)%char = char1
    this%content(2)%char = char2
    this%content(3)%char = char3
    this%content(4)%char = char4
    this%content(5)%char = char5

end function

! #####################################################

! #####################################################

function to_list_6_listclass(char1,char2,char3,char4,char5,char6) result(this)
    character(*),intent(in) :: char1,char2,char3,char4,char5,char6
    type(List_) :: this

    allocate(this%content(6) )
    this%content(1)%char = char1
    this%content(2)%char = char2
    this%content(3)%char = char3
    this%content(4)%char = char4
    this%content(5)%char = char5
    this%content(6)%char = char6

end function

! #####################################################


! #####################################################

function to_list_7_listclass(char1,char2,char3,char4,char5,char6,char7) result(this)
    character(*),intent(in) :: char1,char2,char3,char4,char5,char6,char7
    type(List_) :: this

    allocate(this%content(7) )
    this%content(1)%char = char1
    this%content(2)%char = char2
    this%content(3)%char = char3
    this%content(4)%char = char4
    this%content(5)%char = char5
    this%content(6)%char = char6
    this%content(7)%char = char7
end function

! #####################################################

! #####################################################
function to_list_int32vec_listclass(int32vec) result(this)
    integer(int32),intent(in) :: int32vec(:)
    type(List_) :: this
    integer(int32) :: i
    character(len=50):: b


    allocate(this%content(size(int32vec) )  )
    do i=1,size(int32vec)
        write(b,*) int32vec(i)
        this%content(i)%char = trim(adjustl(b))
    enddo

end function
! #####################################################




! #####################################################
function to_list_real32vec_listclass(real32vec) result(this)
    real(real32),intent(in) :: real32vec(:)
    type(List_) :: this
    integer(int32) :: i
    character(len=50):: b


    allocate(this%content(size(real32vec) )  )
    do i=1,size(real32vec)
        write(b,'(f0.10)') real32vec(i)
        this%content(i)%char = trim(adjustl(b))
    enddo

end function
! #####################################################


! #####################################################
function to_list_real64vec_listclass(real64vec) result(this)
    real(real64),intent(in) :: real64vec(:)
    type(List_) :: this
    integer(int32) :: i
    character(len=50):: b


    allocate(this%content(size(real64vec) )  )
    do i=1,size(real64vec)
        
        write(b,'(G31.20)')  real64vec(i)
        this%content(i)%char = trim(adjustl(b))
    enddo

end function
! #####################################################


! #####################################################
subroutine help_listclass(this)
    class(List_),intent(in) :: this

    print *, "function   get(this,idx) "
    print *, "subroutine append(this,char) "
    print *, "subroutine new(this,length)"
    print *, "print => print_listclass"
    print *, "size => size_listclass"

end subroutine
! #####################################################

! #####################################################
subroutine split_char_into_list(this,str_val,delimiter) 
    character(*),intent(in) :: str_val,delimiter
    class(List_),intent(inout) :: this
    integer(int32) :: i,n,last_idx,current_idx

    ! split charactor(*) into list
    ! First, count the number of entity
    n = 0
    do i=1,len(str_val)
        if(str_val(i:i)==delimiter )then
            n = n + 1
        endif
    enddo

    
    call this%new(n)
    last_idx = 1
    n = 0
    do current_idx=1,len(str_val)-(len(delimiter)-1)
        if(str_val(current_idx:current_idx+len(delimiter)-1 )==delimiter )then
            n = n + 1
            this%content(n)%char = str_val(last_idx:current_idx-1)
            last_idx = current_idx + 1
        endif
    enddo
    if(index(str_val,delimiter,back=.true.)<len(str_val) )then
        n = index(str_val,delimiter,back=.true.)
        if(trim(str_val(n+1:))/="")then
            call this%append(str_val(n+1:)  )
        endif
    endif
    

end subroutine
! #####################################################



end module ListClass