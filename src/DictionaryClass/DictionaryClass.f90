module DictionaryClass
    use, intrinsic :: iso_fortran_env
    use uuid_module
    use MathClass
    use IOClass
    implicit none

    type ::  Page_
        character(:),allocatable :: charvalue ! type_id = 3
        character(:),allocatable :: key
        integer(int32) :: type_id = 0
        integer(int32) :: IntValue ! type_id=1
        real(real64) :: RealValue  ! type_id=2
        integer(int32),allocatable :: intlist(:)! type_id/=3, 4
        real(real64),allocatable :: realist(:)  ! type_id/=4, 5
        ! if value is in dicts(:), type_id = -1 
    end type

    type :: Dictionary_
        character(len=36) :: uuid=""
        type(Page_),allocatable :: pages(:) 
        logical :: initialized = .false.
        integer(int32) :: num_entity=0
    contains
        procedure :: Init => InitializeDictionary
        procedure :: Input => InputDictionary
        procedure :: Get => GetDictionaryValue
        procedure :: GetPageNum => GetPageNumDictionary 
        procedure :: sizeof => sizeofDictionary
        procedure :: content => contentofDictionary
        procedure :: intlist => intlistofDictionary
        procedure :: intvalue => intvalueofDictionary
        procedure :: realvalue => realvalueofDictionary
        procedure :: show => showDictionary
        procedure :: export => exportDictionary
        procedure :: destroy => destroyDictionary
        procedure :: remove => destroyDictionary
        
        ! python-like api
        ! init by dict()
        procedure,pass :: updateDictionaryInt
        procedure,pass :: updateDictionaryReal64
        procedure,pass :: updateDictionaryChar
        procedure,pass :: updateDictionaryDictionary
        generic :: update => updateDictionaryInt,updateDictionaryReal64,&
            updateDictionaryChar,updateDictionaryDictionary
        procedure :: find => findDictionary
        procedure :: findID => findIDDictionary
        procedure :: to_csv => to_csvDictionary
        procedure :: to_json => to_jsonDictionary
        
    end type
    

    type, extends(Page_) :: FileInfo_
        character*200 :: Path
        character*200 :: DirectoryName
        character*200 :: FileName
        integer(int32):: FileID
    end type


    type :: FileList_
        type(FileInfo_),allocatable:: FileList(:)
    contains
        procedure :: setFilePath
        procedure :: setDirectoryName
        procedure :: setFileName
        
    end type

    interface count
        module procedure count_char_char
    end interface count


    interface split
        module procedure splitChar_Dict
    end interface

    interface from_json
        module procedure from_JSON_IOClass
    end interface 

    interface str
        module procedure str_from_dict,str_from_intvec,str_from_realvec
    end interface

    interface operator(>)
        module procedure findDictionary
    end interface

contains

! ##################################################
subroutine updateDictionaryInt(this,Key, intvalue)
    class(Dictionary_),intent(inout)::this
    character(*),intent(in)  :: key
    integer(int32),intent(in) :: intvalue
    integer(int32) :: found_key(2),n
    type(Dictionary_) :: buf

    if(this%uuid=="")then
        this%uuid = generate_uuid(1)
    endif

    if(this%num_entity+1 >= size(this%pages) )then
        ! copy
        buf = this
        ! initialize
        if(allocated(this%pages) )then
            deallocate(this%pages)
        endif
        allocate(this%pages(buf%num_entity + 1000) )
        this%pages(1:buf%num_entity) = buf%pages(1:buf%num_entity)
    endif

    found_key=this%findID(Key)

    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%intValue = intValue
        this%pages(this%num_entity)%type_id = 1
    else
        n = found_key(1)
        
        this%pages(n)%key = key
        this%pages(n)%intValue = intValue
        this%pages(n)%type_id = 1
    endif

end subroutine
! ##################################################

! ##################################################
subroutine updateDictionaryReal64(this,Key, realValue)
    class(Dictionary_),intent(inout)::this
    character(*),intent(in)  :: key
    real(real64),intent(in) :: realValue
    integer(int32) :: found_key(2),n
    type(Dictionary_) :: buf
    
    if(this%uuid=="")then
        this%uuid = generate_uuid(1)
    endif

    if(this%num_entity+1 >= size(this%pages) )then
        ! copy
        buf = this
        ! initialize
        if(allocated(this%pages) )then
            deallocate(this%pages)
        endif
        allocate(this%pages(buf%num_entity + 1000) )
        this%pages(1:buf%num_entity) = buf%pages(1:buf%num_entity)
    endif


    found_key=this%findID(Key)
    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%realValue = realValue
        this%pages(this%num_entity)%type_id = 2
    else
        n = found_key(1)
        
        this%pages(n)%key = key
        this%pages(n)%realValue = realValue
        this%pages(n)%type_id = 2
    endif



end subroutine
! ##################################################


! ##################################################
subroutine updateDictionaryChar(this,Key, charValue)
    class(Dictionary_),intent(inout)::this
    character(*),intent(in)  :: key
    character(*),intent(in) :: charValue
    integer(int32) :: found_key(2),n
    type(Dictionary_) :: buf

    if(.not.this%initialized)then
        call this%init(1)
        this%num_entity = 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%charValue = charValue
        this%pages(this%num_entity)%type_id = 3
        this%initialized = .true.
        return
    endif

    if(this%uuid=="")then
        this%uuid = generate_uuid(1)
    endif

    if(this%num_entity+1 >= size(this%pages) )then

        ! copy
        buf = this
        ! initialize
        if(allocated(this%pages) )then
            deallocate(this%pages)
        endif
        allocate(this%pages(buf%num_entity + 1000) )
        this%pages(1:buf%num_entity) = buf%pages(1:buf%num_entity)
    endif

    found_key=this%findID(Key)
    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%charValue = charValue
        this%pages(this%num_entity)%type_id = 3
    else
        n = found_key(1)
        this%pages(n)%key = key
        this%pages(n)%charValue = charValue
        this%pages(n)%type_id = 3
    endif



end subroutine
! ##################################################


!! ##################################################
!subroutine updateDictionaryDictionary(this,Key, dictValue)
!    class(Dictionary_),intent(inout)::this
!    character(*),intent(in)  :: key
!    type(Dictionary_),intent(in) :: dictValue
!    integer(int32) :: found_key(2),n
!    type(Dictionary_) :: buf
!    
!    
!    if(this%uuid=="")then
!        this%uuid = generate_uuid(1)
!    endif
!    
!    if(this%num_entity+1 >= size(this%pages) )then
!        ! copy
!        buf = this
!        ! initialize
!        if(allocated(this%pages) )then
!            deallocate(this%pages)
!        endif
!        allocate(this%pages(buf%num_entity + 1000) )
!        this%pages(1:buf%num_entity) = buf%pages(1:buf%num_entity)
!
!        
!        !if(allocated(this%dicts)) then
!        !    deallocate(this%dicts)
!        !endif
!        !allocate(this%dicts(buf%num_entity + 1000) )
!        !this%dicts(1:buf%num_entity) = buf%dicts(1:buf%num_entity)
!
!    endif
!
!    found_key=this%findID(Key)
!
!    if(found_key(1)==0 )then
!        ! new
!        this%num_entity = this%num_entity + 1
!        this%pages(this%num_entity)%key = key
!        this%dicts(this%num_entity) = dictValue
!        this%pages(this%num_entity)%type_id = -1
!    else
!        ! not loaded
!        n = found_key(1)
!        this%pages(n)%key = key
!        this%dicts(n) = dictValue
!        this%pages(n)%type_id = -1
!    endif
!
!
!end subroutine
!! ##################################################


! ##################################################
subroutine updateDictionaryDictionary(this,Key, dictValue)
    class(Dictionary_),intent(inout)::this
    character(*),intent(in)  :: key
    character(:),allocatable :: charValue
    type(Dictionary_),intent(in) :: dictValue
    integer(int32) :: found_key(2),n
    type(Dictionary_) :: buf

    charValue = str(dictValue)

    if(.not.this%initialized)then
        call this%init(1)
        this%num_entity = 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%charValue = charValue
        this%pages(this%num_entity)%type_id = -1
        this%initialized = .true.
        return
    endif

    if(this%uuid=="")then
        this%uuid = generate_uuid(1)
    endif

    if(this%num_entity+1 >= size(this%pages) )then

        ! copy
        buf = this
        ! initialize
        if(allocated(this%pages) )then
            deallocate(this%pages)
        endif
        allocate(this%pages(buf%num_entity + 1000) )
        this%pages(1:buf%num_entity) = buf%pages(1:buf%num_entity)
    endif

    found_key=this%findID(Key)
    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%pages(this%num_entity)%key = key
        this%pages(this%num_entity)%charValue = charValue
        this%pages(this%num_entity)%type_id = -1
    else
        n = found_key(1)
        this%pages(n)%key = key
        this%pages(n)%charValue = charValue
        this%pages(n)%type_id = -1
    endif



end subroutine
! ##################################################




! ##################################################
function to_dict(name) result(ret_dict)
    type(Dictionary_) :: ret_dict
    character(*),intent(in) :: name
    type(IO_) :: f
    integer(int32) :: i, num_col, id_from, id_to
    character(:),allocatable :: line, value_,key_
    
    if("{" .in. name )then
        if( .not.(".json" .in. name))then
            ! parse name as json 
            ret_dict = parse_json_as_dict(name)
            return
        endif
    endif

    ret_dict = dict()
    call f%open(name)
    
    do 
        if(f%EOF)exit
        line = f%readline()
        num_col = count(line,",")
        id_to   = 1
        id_from = 1 
        if(num_col < 2) cycle
        key_ = ""
        value_ = ""
        do i=1, 2
            id_to  = index(line(id_from:),",") 
            
            
            

            if(id_from+id_to-1>len(line) )exit
            if(i==1)then
                key_   = line(id_from:id_to-1)
                
            else
                value_ = line(id_from:id_from+id_to-2)
                
            endif
            id_from = id_to +1
            
        enddo
        key_ = key_(2:)
        key_ = key_(:len(key_)-1 )
        
        if(index(value_,".")/=0 )then
            call ret_dict%update(key_,freal(value_))
        else
            call ret_dict%update(key_,fint(value_))
        endif
    enddo
    call f%close()

end function
! ##############################################



! ##################################################
function dict(max_num_entity) result(ret_dict)
    type(Dictionary_) :: ret_dict
    integer(int32),optional,intent(in) :: max_num_entity
    integer(int32) :: max_entity

    max_entity = 1000
    if(present(max_num_entity) )then
        max_entity = max_num_entity
    endif

    ret_dict%initialized = .true.
    allocate(ret_dict%pages(max_entity))

end function
! ##############################################

subroutine InitializeDictionary(obj,NumOfPage)
    class(Dictionary_),intent(inout)::obj
    integer(int32),intent(in)      :: NumOfPage

    
    if(allocated(obj%pages) )then
        deallocate(obj%pages)
    endif
    allocate(obj%pages(NumOfPage) )

    obj%initialized = .true.
    obj%uuid = generate_uuid(1)
    
end subroutine
! ##############################################


! ##############################################
subroutine InputDictionary(obj,page,content,RealValue,IntValue,Realist,Intlist,DictValue)
    class(Dictionary_),intent(inout)::obj
    integer(int32),intent(in)      :: page
    character(*),optional,intent(in)           :: Content
    integer(int32),optional,intent(in) :: IntValue,Intlist(:)
    real(real64),optional,intent(in) :: RealValue,Realist(:)
    type(Dictionary_),optional,intent(in) :: DictValue

    if(page > size(obj%pages) )then
        print *, "Error :: InputDictionary >> Num of Page is overflowed"
        stop
    endif
    if(present(RealValue)  )then
        obj%pages(page)%RealValue = RealValue
        return
    endif
    if(present(Realist)  )then
        if(allocated(obj%pages(page)%Realist) )then
            deallocate(obj%pages(page)%Realist)
        endif
        allocate(obj%pages(page)%Realist(size(Realist,1) ) )
        obj%pages(page)%Realist(:) = Realist(:)
        return
    endif
    if(present(IntValue)  )then
        obj%pages(page)%intValue = intValue
        return
    endif
    if(present(intlist)  )then
        if(allocated(obj%pages(page)%intlist) )then
            deallocate(obj%pages(page)%intlist)
        endif
        allocate(obj%pages(page)%intlist(size(intlist,1) ) )
        obj%pages(page)%intlist(:) = intlist(:)
        return
    endif

    if(present(content) )then
        obj%pages(page)%charValue = content 
    endif
end subroutine
! ##############################################



! ##############################################
function intlistofDictionary(obj,page,ind) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page,ind
    integer(int32) :: n

    n=obj%pages(page)%intlist(ind)

end function
! ##############################################

! ##############################################
function intvalueofDictionary(obj,page) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page
    integer(int32) :: n

    n=obj%pages(page)%intvalue

end function
! ##############################################



! ##############################################
function realvalueofDictionary(obj,page) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page
    real(real64) :: n

    n=obj%pages(page)%realvalue

end function
! ##############################################



! ##############################################
function GetDictionaryValue(obj,page) result(content)
    class(Dictionary_),intent(in)::obj
    integer(int32),intent(in)      :: page
    character(:),allocatable :: content

    if(page > size(obj%pages) ) then
        content = ""
        return
    endif
    content = obj%pages(page)%charValue

end function
! ##############################################

! ##############################################
subroutine setFilePath(obj,FilePath,FileID) 
    class(FileList_),intent(inout)::obj
    integer(int32),intent(in) :: FileID
    character*200,intent(in) :: FilePath

    obj%FileList(FileID)%Path = FilePath

end subroutine
! ##############################################

! ##############################################
subroutine setDirectoryName(obj,DirectoryName,FileID) 
    class(FileList_),intent(inout)::obj
    integer(int32),intent(in) :: FileID
    character*200,intent(in) :: DirectoryName

    obj%FileList(FileID)%DirectoryName = DirectoryName

end subroutine
! ##############################################


! ##############################################
subroutine setFileName(obj,FileName,FileID) 
    class(FileList_),intent(inout)::obj
    integer(int32),intent(in) :: FileID
    character*200,intent(in) :: FileName

    obj%FileList(FileID)%FileName = FileName

end subroutine
! ##############################################


! ##############################################
subroutine showDictionary(obj,From,to,Name)
    class(Dictionary_)::obj
    integer(int32),optional,intent(in)::From,to
    character(*),optional,intent(in) :: Name
    integer(int32) :: i,n,startp,endp,rl,il

    n=obj%num_entity

    
    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)

    
    do i=startp,endp
        rl = 0
        il = 0
        if(.not. allocated(obj%pages(i)%Intlist) )then
            allocate(obj%pages(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%pages(i)%Realist) )then
            allocate(obj%pages(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%pages(i)%type_id==1)then
            print *, '{"'+obj%pages(i)%Key +'":',&
                str(obj%pages(i)%IntValue)+"}"
        elseif(obj%pages(i)%type_id==2)then
            print *, '{"'+obj%pages(i)%Key +'":',&
            str(obj%pages(i)%realValue)+"}"
        elseif(obj%pages(i)%type_id==3)then
            print *, '{"'+obj%pages(i)%Key +'":',&
            obj%pages(i)%charValue+"}"
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%pages(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%pages(i)%Realist )
        endif
    enddo
    

    if(present(Name) )then
        open(1023,file=Name)
        
        
        do i=startp,endp
            rl = 0
            il = 0
            if(.not. allocated(obj%pages(i)%Intlist) )then
                allocate(obj%pages(i)%Intlist(0) )
                il = 1
            endif
            if(.not. allocated(obj%pages(i)%Realist) )then
                allocate(obj%pages(i)%Realist(0) )
                rl = 1
            endif
            write(1023,*) "Page : ",i,"Content : ",obj%pages(i)%charValue ,&
                "IntValue : ",obj%pages(i)%IntValue,&
                "RealValue : ",obj%pages(i)%RealValue,&
                "Intlist(:) : ",obj%pages(i)%Intlist(:),&
                "Realist(:) : ",obj%pages(i)%Realist(:)
            
            if(il==1 )then
                deallocate(obj%pages(i)%Intlist )
            endif
            if(rl == 1 )then
                deallocate(obj%pages(i)%Realist )
            endif
        enddo
        close(1023)

    endif


    
end subroutine
! ##############################################

! ##############################################
subroutine exportDictionary(obj,FileName,fh,from,to)
    class(Dictionary_)::obj
    integer(int32),optional,intent(in)::From,to,fh
    character(*),intent(in) :: FileName
    integer(int32) :: i,n,startp,endp,rl,il,nnn

    n=size(obj%pages,1)
    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)

    nnn=input(default=1000,option=fh)
    open(nnn,file=FileName)
    
    
    do i=startp,endp
        rl = 0
        il = 0
        if(.not. allocated(obj%pages(i)%Intlist) )then
            allocate(obj%pages(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%pages(i)%Realist) )then
            allocate(obj%pages(i)%Realist(0) )
            rl = 1
        endif
        write(nnn,*) "Page : ",i,"Content : ",obj%pages(i)%charValue ,&
            "IntValue : ",obj%pages(i)%IntValue,&
            "RealValue : ",obj%pages(i)%RealValue,&
            "Intlist(:) : ",obj%pages(i)%Intlist(:),&
            "Realist(:) : ",obj%pages(i)%Realist(:)
        
        if(il==1 )then
            deallocate(obj%pages(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%pages(i)%Realist )
        endif
    enddo
    close(nnn)

end subroutine
! ##############################################

! ##############################################
function sizeofDictionary(obj) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32) :: n

    n = size(obj%pages)

end function
! ##############################################


! ##############################################
function contentofDictionary(obj,id) result(content)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: id
    character*200 :: content
 
    content = obj%pages(id)%charvalue

end function
! ##############################################



! ##############################################
function GetPageNumDictionary(obj,Content) result(page)
    class(Dictionary_),intent(in)::obj
    character(*),intent(in)::Content
    integer(int32) :: page
    integer(int32) :: i,n

    n=size(obj%pages,1)
    page=-1
    do i=1,n
        if(Content==obj%pages(i)%charvalue )then
            page=i
            return
        endif
    enddo
    if(page==-1)then
        print *, "ERROR ::",Content," is a word to be found only in the dictionary of fools."
    endif



end function
! ##############################################

! ##############################################
subroutine destroyDictionary(obj)
    class(Dictionary_),intent(inout) :: obj

    if(allocated(obj%pages))then
        deallocate(obj%pages)
    endif
    obj%initialized = .false.
end subroutine
! ##############################################


! ##############################################
recursive function findDictionary(this,key) result(val)
    class(Dictionary_),intent(in) :: this
    character(*),intent(in) :: key
    character(:),allocatable :: val
    integer(int32) :: i
    
    do i=1,this%num_entity
        if(this%pages(i)%key == Key  )then
            ! Found!
            select case (this%pages(i)%type_id)
                case(-1)
                    val = this%pages(i)%charValue
                    return
                case(1)
                    val = str(this%pages(i)%intValue)
                    return
                case(2)
                    val = str(this%pages(i)%realValue)
                    return
                case(3)
                    val = this%pages(i)%charValue
                    return
            end select
        endif
    enddo
    
    val = "__None__"
    
end function
! ##############################################

! ##############################################
recursive function findIDDictionary(this,key) result(val)
    class(Dictionary_),intent(in) :: this
    character(*),intent(in) :: key
    integer(int32) :: val(2)
    integer(int32) :: i
    
    do i=1,this%num_entity
        if(this%pages(i)%key == Key  )then
            ! Found!
            val(1) = i
            val(2) = this%pages(i)%type_id
            return
        endif
    enddo
    
    val = 0
    
end function

subroutine to_csvDictionary(obj,Name,from,to)
    class(Dictionary_)::obj
    integer(int32),optional,intent(in)::From,to
    character(*),intent(in) :: Name
    type(IO_) :: f
    integer(int32) :: i,n,startp,endp,rl,il

    n=obj%num_entity

    
    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)

    call f%open(Name+".csv")
    do i=startp,endp
        rl = 0
        il = 0
        if(.not. allocated(obj%pages(i)%Intlist) )then
            allocate(obj%pages(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%pages(i)%Realist) )then
            allocate(obj%pages(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%pages(i)%type_id==1)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'",',&
                str(obj%pages(i)%IntValue)+","
        elseif(obj%pages(i)%type_id==2)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'",',&
            str(obj%pages(i)%realValue)+","
        elseif(obj%pages(i)%type_id==3)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'",',&
            obj%pages(i)%charvalue+","
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%pages(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%pages(i)%Realist )
        endif
    enddo
    
    call f%close()

end subroutine


subroutine to_jsonDictionary(obj,Name,from,to)
    class(Dictionary_)::obj
    integer(int32),optional,intent(in)::From,to
    character(*),intent(in) :: Name
    type(IO_) :: f
    integer(int32) :: i,n,startp,endp,rl,il

    n=obj%num_entity

    
    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)

    call f%open(Name+".json")
    call f%write("{")
    do i=startp,endp-1
        rl = 0
        il = 0
        if(.not. allocated(obj%pages(i)%Intlist) )then
            allocate(obj%pages(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%pages(i)%Realist) )then
            allocate(obj%pages(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%pages(i)%type_id==1)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
                str(obj%pages(i)%IntValue)+","
        elseif(obj%pages(i)%type_id==2)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
            str(obj%pages(i)%realValue)+","
        elseif(obj%pages(i)%type_id==3)then
            write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
            obj%pages(i)%charValue+","
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%pages(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%pages(i)%Realist )
        endif
    enddo
    i=endp
    if(obj%pages(i)%type_id==1)then
        write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
            str(obj%pages(i)%IntValue)
    elseif(obj%pages(i)%type_id==2)then
        write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
        str(obj%pages(i)%realValue)
    elseif(obj%pages(i)%type_id==3)then
        write(f%fh,*) '"'+obj%pages(i)%Key +'": ',&
        obj%pages(i)%charValue
    else
        ! do nothing
    endif

    call f%write("}")
    call f%close()

end subroutine
! ###########################################################


! ###########################################################
recursive function count_char_char(sentence, key, initialized) result(ret)
    character(*),intent(in) :: sentence
    character(*),intent(in) :: key
    character(:),allocatable :: small_sentence
    logical,optional,intent(in) :: initialized
    integer(int32) :: n_len, k
    integer(int32) :: ret

    n_len = len(sentence)
    if(present(initialized) )then
        if(initialized)then
            ! nothing to do
        else
            ret = 0.0d0
        endif
    else
        ret = 0.0d0
    endif
    
    if(index(sentence,key)==0 ) then
        return
    else
        ret = ret + 1
        if(index(sentence,key)+1>n_len) return
        small_sentence = sentence(index(sentence,key)+1:)
        ret = ret + &
            count_char_char(sentence=small_sentence,key=key,initialized=.true.)
    endif

end function
! ###########################################################


! ###########################################################
function splitChar_Dict(line,splitter) result(ret_dict)
    character(*),intent(in) :: line,splitter
    integer(int32) :: i, n, from
    type(Dictionary_) :: ret_dict

    n = count(line,splitter)
    call ret_dict%init(n+1)
    
    from = 1
    ret_dict%pages(1)%charvalue = line(:index(line,splitter)-1 )
    from = from + len(ret_dict%pages(1)%charvalue) + len(splitter)
    do i=2,n
        ret_dict%pages(i)%charvalue = line(:index(line(from:),splitter)-1 )
        from = from + len(ret_dict%pages(1)%charvalue) + len(splitter)
    enddo
    ret_dict%pages(n+1)%charvalue = line(from:)

end function
! ###########################################################



! #####################################################################
recursive function from_JSON_IOClass(filename,from_line_idx,only_num_bracket) result(ret)
    character(*),intent(in) :: filename
    integer(int32),optional,intent(in) :: from_line_idx,only_num_bracket
    
    type(IO_) :: f,debug
    type(Dictionary_) :: ret, mini_dict
    type(List_) :: key_and_value
    character(:),allocatable :: line
    integer(int32) :: num_bracket,line_idx

    call ret%init(NumOfPage=get_num_entity_json(filename))
    call debug%open("debug.txt","w")
    call f%open(filename,"r")
    num_bracket = 0
    line_idx=0
    do
        line_idx = line_idx + 1
        if(f%EOF)exit
        if(present(only_num_bracket) )then
            if(num_bracket /= only_num_bracket) exit
        endif
        if(present(from_line_idx) )then
            if(from_line_idx > line_idx)then
                cycle
            endif
        endif

        line = f%readline()
        if(num_bracket==1 .and. (":" .in. line)  )then
            ! primary layer
            call replace(line,"'","")
            call replace(line,'"',"")
            !call replace(line," ","")
            call replace(line,",","")
            call key_and_value%split(line,delimiter=":")
            call debug%write(line)
            call ret%update(key_and_value%get(1),key_and_value%get(2))
        elseif(num_bracket>=2)then
            ! nested 
            mini_dict = from_JSON_IOClass(filename,from_line_idx=line_idx,only_num_bracket=num_bracket)
            !key_and_value%content(2)%char = key_and_value%get(2) + line
            call ret%update(key_and_value%get(1),mini_dict)
        endif 

        if ( "{" .in. line )then
            num_bracket = num_bracket + 1
            ! 
        endif
        if ( "}" .in. line )then
            num_bracket = num_bracket - 1
        endif
        if(len(trim(line))==0 )then
            call debug%write("[space]")
            cycle
        else
            call debug%write(str(line_idx)+" "+str(num_bracket))
        endif

        
    enddo
    call debug%close()
    call f%close()

end function
! #####################################################################

function get_num_entity_json(filename) result(ret)
    type(IO_) :: f,debug
    character(*),intent(in) :: filename
    integer(int32) :: ret
    character(:),allocatable :: line
    integer(int32) :: num_bracket,line_idx

    ret = 0
    call f%open(filename,"r")
    num_bracket = 0
    line_idx=0
    do
        line_idx = line_idx + 1
        if(f%EOF)exit

        line = f%readline()

        if(num_bracket==1)then
            ! primary layer
            ret = ret + 1
        endif 
        
        if ( "{" .in. line )then
            num_bracket = num_bracket + 1
            ! 
        endif
        if ( "}" .in. line )then
            num_bracket = num_bracket - 1
        endif
        if(len(trim(line))==0 )then
            cycle
        endif
    enddo
    call f%close()

end function
! #####################################################################

! #####################################################################

recursive function str_from_dict(dict) result(ret)
    type(Dictionary_),intent(in) :: dict
    character(:),allocatable :: ret
    integer(int32) :: i

    ret = ""
    if(.not.allocated(dict%pages) ) then
        return
    endif
    if(size(dict%pages)==0 ) then
        return
    endif

    ret = ret + '{'+new_line("A")
    do i=1,dict%num_entity
        ret = ret +'"' + dict%pages(i)%key
        ret = ret + '":'
        if(dict%pages(i)%type_id==-1)then
            ! Dictionary型
            ret = ret + dict%pages(i)%charValue
        elseif(dict%pages(i)%type_id==3)then
            ! 文字列
            ret = ret + '"' + dict%pages(i)%charValue + '"' 
        elseif(dict%pages(i)%type_id==1)then
            ret = ret + str(dict%pages(i)%intValue)
        elseif(dict%pages(i)%type_id==2)then
            ret = ret + str(dict%pages(i)%RealValue)
        elseif(dict%pages(i)%type_id==4)then
            ret = ret + str(dict%pages(i)%intlist)
        elseif(dict%pages(i)%type_id==5)then
            ret = ret + str(dict%pages(i)%Realist)
        endif
        if(i <= dict%num_entity-1)then
            ret = ret + ","
        endif
        ret = ret + new_line("A")
    enddo
    ret = ret + '}'

end function
! #####################################################################

! #####################################################################
function str_from_intvec(intvec) result(ret)
    integer(int32),intent(in) :: intvec(:)
    character(:),allocatable :: ret
    integer(int32) :: i

    ret = "["
    do i=1,size(intvec)
        ret = ret + str(intvec(i))+","
    enddo
    ret = ret + "]"
end function
! #####################################################################


! #####################################################################
function str_from_realvec(realvec) result(ret)
    real(real64),intent(in) :: realvec(:)
    character(:),allocatable :: ret
    integer(int32) :: i

    ret = "["
    do i=1,size(realvec)
        ret = ret + str(realvec(i))+","
    enddo
    ret = ret + "]"
end function
! #####################################################################


! #####################################################################
! json文字列をlistにして返す．
!-----------------------------
recursive function parse_json_as_dict(content,from_line_idx,only_num_bracket) result(ret)
character(*),intent(in) :: content
integer(int32),optional,intent(in) :: from_line_idx,only_num_bracket
type(IO_) :: debug
type(Dictionary_) :: ret, mini_dict
type(List_) :: key_and_value
character(:),allocatable :: line,content_val
integer(int32) :: num_bracket,line_idx,num_bracket_L1

! pre-processing
if(.not. ( new_line("A") .in. content ) )then
    content_val = content
    call replace(content_val,"{","{"+new_line("A"))
    call replace(content_val,"}",new_line("A")+"}")
    ret = parse_json_as_dict(content_val,from_line_idx,only_num_bracket)
    return
endif


call ret%init(NumOfPage=get_num_entity_json_str(content)) !!!
call debug%open("debug.txt","w")
!call f%open(filename,"r")
num_bracket = 0
line_idx=0
do
    line_idx = line_idx + 1
    if(is_EOL(content,line_idx) )exit !!!
    
    if(present(from_line_idx) )then
        if(from_line_idx > line_idx)then
            cycle
        endif
    endif

    if(present(only_num_bracket) )then
        if(num_bracket < only_num_bracket) exit
    endif

    line = readline(content,line_idx) !!!
    
    if(num_bracket==1 .and. (":" .in. line)  )then
        if( ("{" .in. line) .and. index(line,"{") > index(line,":") )then
            ! primary layer
            call replace(line,"'","")
            call replace(line,'"',"")
            call replace(line,",","")
            call key_and_value%split(line,delimiter=":")
            num_bracket_L1 = 1
            do
                if(num_bracket_L1==0)exit
                line_idx = line_idx + 1
                line = readline(content,line_idx) 
                key_and_value%content(2)%char = key_and_value%content(2)%char +new_line("A") &
                    +line
                
                if ( "{" .in. line )then
                    num_bracket_L1 = num_bracket_L1 + 1
                    ! 
                endif
                if ( "}" .in. line )then
                    num_bracket_L1 = num_bracket_L1 - 1
                endif
            enddo
            call debug%write("debug L1 >> "+ str(to_dict(key_and_value%get(2))))
            !call debug%write("end mini_dict #1>> "+str(mini_dict))
            !key_and_value%content(2)%char = key_and_value%get(2) + line
            !call ret%update(key_and_value%get(1),key_and_value%get(2))
            call ret%update(key_and_value%get(1),to_dict(key_and_value%get(2)))
            !call debug%write("debug >> "+key_and_value%get(1)+", "+key_and_value%get(2))
            !call ret%update(key_and_value%get(1),key_and_value%get(2))
            call debug%write("debug Num_line L1 >> "+ str(line_idx))
            cycle
        else
            ! primary layer
            call replace(line,"'","")
            call replace(line,'"',"")
            call replace(line,",","")
            call key_and_value%split(line,delimiter=":")
            call debug%write("debug L0 >> "+key_and_value%get(1)+", "+key_and_value%get(2))
            call ret%update(key_and_value%get(1),key_and_value%get(2))
        endif
    elseif(num_bracket>=2)then
        ! nested 
        call debug%write("mini_dict >> "+str(mini_dict))
        mini_dict = parse_json_as_dict(content,from_line_idx=line_idx,&
            only_num_bracket=num_bracket)
        call debug%write("end mini_dict >> "+str(mini_dict))
        !key_and_value%content(2)%char = key_and_value%get(2) + line
        call ret%update(key_and_value%get(1),mini_dict)
    endif 

    if ( "{" .in. line )then
        num_bracket = num_bracket + 1
        ! 
    endif
    if ( "}" .in. line )then
        num_bracket = num_bracket - 1
    endif
    if(len(trim(line))==0 )then
        call debug%write("[space]")
        cycle
    else
        call debug%write(str(line_idx)+" "+str(num_bracket))
    endif

    
enddo
call debug%close()

end function
! #####################################################################

! #####################################################################
function get_num_entity_json_str(content) result(ret)
    character(*),intent(in) :: content

    ! contentにentityが何個あるかを数え上げる．
    ! contentは改行を含む．(filename) result(ret)
    type(IO_) :: f,debug
    integer(int32) :: ret
    character(:),allocatable :: line
    integer(int32) :: num_bracket,line_idx

    ret = 0
    !call f%open(filename,"r")
    num_bracket = 0
    line_idx=0
    do
        line_idx = line_idx + 1
        if(is_EOL(content,line_idx) )exit

        line = readline(content,line_idx)

        if(num_bracket==1)then
            ! primary layer
            ret = ret + 1
        endif 


        
        if ( "{" .in. line )then
            num_bracket = num_bracket + 1
            ! 
        endif
        if ( "}" .in. line )then
            num_bracket = num_bracket - 1
            if(num_bracket==0)then
                ret = ret - 1
            endif
        endif
        if(len(trim(line))==0 )then
            cycle
        endif
    enddo
    !call f%close()



end function
! #####################################################################

! #####################################################################
function is_EOL(content,line_idx) result(ret)
    character(*),intent(in) :: content
    integer(int32),intent(in) :: line_idx
    type(List_) :: list_buf
    logical :: ret

    call list_buf%split(content,new_line("A"))
    ! End Of Linesかどうか調べる．
    if(line_idx <= list_buf%size())then
        ret = .false.
    else
        ret = .true.
    endif

end function
! #####################################################################


! #####################################################################
function readline(content,line_idx) result(ret)
    character(*),intent(in) :: content
    integer(int32),intent(in) :: line_idx
    character(:),allocatable :: ret
    type(List_) :: list

    ! 特定行を読む．
    if(is_EOL(content,line_idx) )then
        ret = ""
    else
        call list%split(content,new_line("A"))
        ret = list%get(line_idx)
    endif
end function
! #####################################################################

end module