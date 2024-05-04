module DictionaryClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use IOClass
    implicit none

    type ::  Page_
        character(:),allocatable :: charvalue
        character(:),allocatable :: key
        integer(int32) :: type_id = 0
        integer(int32) :: IntValue ! type_id=1
        real(real64) :: RealValue  ! type_id=2
        integer(int32),allocatable :: intlist(:)! type_id=3
        real(real64),allocatable :: realist(:)  ! type_id=4
    end type

    type :: Dictionary_
        type(Page_),allocatable :: Dictionary(:)
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
        generic :: update => updateDictionaryInt,updateDictionaryReal64,&
            updateDictionaryChar
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
contains

! ##################################################
subroutine updateDictionaryInt(this,Key, intvalue)
    class(Dictionary_),intent(inout)::this
    character(*),intent(in)  :: key
    integer(int32),intent(in) :: intvalue
    integer(int32) :: found_key(2),n
    type(Dictionary_) :: buf

    if(this%num_entity+1 >= size(this%Dictionary) )then
        ! copy
        buf = this
        ! initialize
        deallocate(this%Dictionary)
        allocate(this%Dictionary(buf%num_entity + 1000) )
        this%Dictionary(1:buf%num_entity) = buf%Dictionary(1:buf%num_entity)
    endif

    found_key=this%findID(Key)

    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%Dictionary(this%num_entity)%key = key
        this%Dictionary(this%num_entity)%intValue = intValue
        this%Dictionary(this%num_entity)%type_id = 1
    else
        n = found_key(1)
        
        this%Dictionary(n)%key = key
        this%Dictionary(n)%intValue = intValue
        this%Dictionary(n)%type_id = 1
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

    if(this%num_entity+1 >= size(this%Dictionary) )then
        ! copy
        buf = this
        ! initialize
        deallocate(this%Dictionary)
        allocate(this%Dictionary(buf%num_entity + 1000) )
        this%Dictionary(1:buf%num_entity) = buf%Dictionary(1:buf%num_entity)
    endif


    found_key=this%findID(Key)
    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%Dictionary(this%num_entity)%key = key
        this%Dictionary(this%num_entity)%realValue = realValue
        this%Dictionary(this%num_entity)%type_id = 2
    else
        n = found_key(1)
        
        this%Dictionary(n)%key = key
        this%Dictionary(n)%realValue = realValue
        this%Dictionary(n)%type_id = 2
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

    if(this%num_entity+1 >= size(this%Dictionary) )then
        ! copy
        buf = this
        ! initialize
        deallocate(this%Dictionary)
        allocate(this%Dictionary(buf%num_entity + 1000) )
        this%Dictionary(1:buf%num_entity) = buf%Dictionary(1:buf%num_entity)
    endif

    found_key=this%findID(Key)
    if(found_key(1)==0 )then
        this%num_entity = this%num_entity + 1
        this%Dictionary(this%num_entity)%key = key
        this%Dictionary(this%num_entity)%charValue = charValue
        this%Dictionary(this%num_entity)%type_id = 3
    else
        n = found_key(1)
        
        this%Dictionary(n)%key = key
        this%Dictionary(n)%charValue = charValue
        this%Dictionary(n)%type_id = 3
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
    allocate(ret_dict%Dictionary(max_entity))

end function
! ##############################################

subroutine InitializeDictionary(obj,NumOfPage)
    class(Dictionary_),intent(inout)::obj
    integer(int32),intent(in)      :: NumOfPage
    if(allocated(obj%Dictionary) )then
        deallocate(obj%Dictionary)
    endif
    allocate(obj%Dictionary(NumOfPage) )
    obj%initialized = .true.

end subroutine
! ##############################################


! ##############################################
subroutine InputDictionary(obj,page,content,RealValue,IntValue,Realist,Intlist)
    class(Dictionary_),intent(inout)::obj
    integer(int32),intent(in)      :: page
    character(*),optional,intent(in)           :: Content
    integer(int32),optional,intent(in) :: IntValue,Intlist(:)
    real(real64),optional,intent(in) :: RealValue,Realist(:)
    
    if(page > size(obj%Dictionary) )then
        print *, "Error :: InputDictionary >> Num of Page is overflowed"
        stop
    endif
    if(present(RealValue)  )then
        obj%Dictionary(page)%RealValue = RealValue
        return
    endif
    if(present(Realist)  )then
        if(allocated(obj%Dictionary(page)%Realist) )then
            deallocate(obj%Dictionary(page)%Realist)
        endif
        allocate(obj%Dictionary(page)%Realist(size(Realist,1) ) )
        obj%Dictionary(page)%Realist(:) = Realist(:)
        return
    endif
    if(present(IntValue)  )then
        obj%Dictionary(page)%intValue = intValue
        return
    endif
    if(present(intlist)  )then
        if(allocated(obj%Dictionary(page)%intlist) )then
            deallocate(obj%Dictionary(page)%intlist)
        endif
        allocate(obj%Dictionary(page)%intlist(size(intlist,1) ) )
        obj%Dictionary(page)%intlist(:) = intlist(:)
        return
    endif

    if(present(content) )then
        obj%Dictionary(page)%charValue = content 
    endif
end subroutine
! ##############################################



! ##############################################
function intlistofDictionary(obj,page,ind) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page,ind
    integer(int32) :: n

    n=obj%Dictionary(page)%intlist(ind)

end function
! ##############################################

! ##############################################
function intvalueofDictionary(obj,page) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page
    integer(int32) :: n

    n=obj%Dictionary(page)%intvalue

end function
! ##############################################



! ##############################################
function realvalueofDictionary(obj,page) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: page
    real(real64) :: n

    n=obj%Dictionary(page)%realvalue

end function
! ##############################################



! ##############################################
function GetDictionaryValue(obj,page) result(content)
    class(Dictionary_),intent(in)::obj
    integer(int32),intent(in)      :: page
    character(:),allocatable :: content

    if(page > size(obj%Dictionary) ) then
        content = ""
        return
    endif
    content = obj%Dictionary(page)%charValue

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
        if(.not. allocated(obj%Dictionary(i)%Intlist) )then
            allocate(obj%Dictionary(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%Dictionary(i)%Realist) )then
            allocate(obj%Dictionary(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%Dictionary(i)%type_id==1)then
            print *, '{"'+obj%Dictionary(i)%Key +'":',&
                str(obj%Dictionary(i)%IntValue)+"}"
        elseif(obj%Dictionary(i)%type_id==2)then
            print *, '{"'+obj%Dictionary(i)%Key +'":',&
            str(obj%Dictionary(i)%realValue)+"}"
        elseif(obj%Dictionary(i)%type_id==3)then
            print *, '{"'+obj%Dictionary(i)%Key +'":',&
            obj%Dictionary(i)%charValue+"}"
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%Dictionary(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%Dictionary(i)%Realist )
        endif
    enddo
    

    if(present(Name) )then
        open(1023,file=Name)
        
        
        do i=startp,endp
            rl = 0
            il = 0
            if(.not. allocated(obj%Dictionary(i)%Intlist) )then
                allocate(obj%Dictionary(i)%Intlist(0) )
                il = 1
            endif
            if(.not. allocated(obj%Dictionary(i)%Realist) )then
                allocate(obj%Dictionary(i)%Realist(0) )
                rl = 1
            endif
            write(1023,*) "Page : ",i,"Content : ",obj%Dictionary(i)%charValue ,&
                "IntValue : ",obj%Dictionary(i)%IntValue,&
                "RealValue : ",obj%Dictionary(i)%RealValue,&
                "Intlist(:) : ",obj%Dictionary(i)%Intlist(:),&
                "Realist(:) : ",obj%Dictionary(i)%Realist(:)
            
            if(il==1 )then
                deallocate(obj%Dictionary(i)%Intlist )
            endif
            if(rl == 1 )then
                deallocate(obj%Dictionary(i)%Realist )
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

    n=size(obj%Dictionary,1)
    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)

    nnn=input(default=1000,option=fh)
    open(nnn,file=FileName)
    
    
    do i=startp,endp
        rl = 0
        il = 0
        if(.not. allocated(obj%Dictionary(i)%Intlist) )then
            allocate(obj%Dictionary(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%Dictionary(i)%Realist) )then
            allocate(obj%Dictionary(i)%Realist(0) )
            rl = 1
        endif
        write(nnn,*) "Page : ",i,"Content : ",obj%Dictionary(i)%charValue ,&
            "IntValue : ",obj%Dictionary(i)%IntValue,&
            "RealValue : ",obj%Dictionary(i)%RealValue,&
            "Intlist(:) : ",obj%Dictionary(i)%Intlist(:),&
            "Realist(:) : ",obj%Dictionary(i)%Realist(:)
        
        if(il==1 )then
            deallocate(obj%Dictionary(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%Dictionary(i)%Realist )
        endif
    enddo
    close(nnn)

end subroutine
! ##############################################

! ##############################################
function sizeofDictionary(obj) result(n)
    class(Dictionary_),intent(in) :: obj
    integer(int32) :: n

    n = size(obj%Dictionary)

end function
! ##############################################


! ##############################################
function contentofDictionary(obj,id) result(content)
    class(Dictionary_),intent(in) :: obj
    integer(int32),intent(in) :: id
    character*200 :: content
 
    content = obj%Dictionary(id)%charvalue

end function
! ##############################################



! ##############################################
function GetPageNumDictionary(obj,Content) result(page)
    class(Dictionary_),intent(in)::obj
    character(*),intent(in)::Content
    integer(int32) :: page
    integer(int32) :: i,n

    n=size(obj%Dictionary,1)
    page=-1
    do i=1,n
        if(Content==obj%Dictionary(i)%charvalue )then
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

    if(allocated(obj%dictionary))then
        deallocate(obj%Dictionary)
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
        if(this%Dictionary(i)%key == Key  )then
            ! Found!
            select case (this%Dictionary(i)%type_id)
                case(1)
                    val = str(this%Dictionary(i)%intValue)
                    return
                case(2)
                    val = str(this%Dictionary(i)%realValue)
                    return
                case(3)
                    val = this%Dictionary(i)%charValue
                    return
            end select
        endif
    enddo
    
    val = "__None__"
    
end function

! ##############################################
recursive function findIDDictionary(this,key) result(val)
    class(Dictionary_),intent(in) :: this
    character(*),intent(in) :: key
    integer(int32) :: val(2)
    integer(int32) :: i
    
    do i=1,this%num_entity
        if(this%Dictionary(i)%key == Key  )then
            ! Found!
            val(1) = i
            val(2) = this%Dictionary(i)%type_id
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
        if(.not. allocated(obj%Dictionary(i)%Intlist) )then
            allocate(obj%Dictionary(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%Dictionary(i)%Realist) )then
            allocate(obj%Dictionary(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%Dictionary(i)%type_id==1)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'",',&
                str(obj%Dictionary(i)%IntValue)+","
        elseif(obj%Dictionary(i)%type_id==2)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'",',&
            str(obj%Dictionary(i)%realValue)+","
        elseif(obj%Dictionary(i)%type_id==3)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'",',&
            obj%Dictionary(i)%charvalue+","
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%Dictionary(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%Dictionary(i)%Realist )
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
        if(.not. allocated(obj%Dictionary(i)%Intlist) )then
            allocate(obj%Dictionary(i)%Intlist(0) )
            il = 1
        endif
        if(.not. allocated(obj%Dictionary(i)%Realist) )then
            allocate(obj%Dictionary(i)%Realist(0) )
            rl = 1
        endif
    
        if(obj%Dictionary(i)%type_id==1)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
                str(obj%Dictionary(i)%IntValue)+","
        elseif(obj%Dictionary(i)%type_id==2)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
            str(obj%Dictionary(i)%realValue)+","
        elseif(obj%Dictionary(i)%type_id==3)then
            write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
            obj%Dictionary(i)%charValue+","
        else
            ! do nothing
        endif
        
        if(il==1 )then
            deallocate(obj%Dictionary(i)%Intlist )
        endif
        if(rl == 1 )then
            deallocate(obj%Dictionary(i)%Realist )
        endif
    enddo
    i=endp
    if(obj%Dictionary(i)%type_id==1)then
        write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
            str(obj%Dictionary(i)%IntValue)
    elseif(obj%Dictionary(i)%type_id==2)then
        write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
        str(obj%Dictionary(i)%realValue)
    elseif(obj%Dictionary(i)%type_id==3)then
        write(f%fh,*) '"'+obj%Dictionary(i)%Key +'": ',&
        obj%Dictionary(i)%charValue
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
    ret_dict%Dictionary(1)%charvalue = line(:index(line,splitter)-1 )
    from = from + len(ret_dict%Dictionary(1)%charvalue) + len(splitter)
    do i=2,n
        ret_dict%Dictionary(i)%charvalue = line(:index(line(from:),splitter)-1 )
        from = from + len(ret_dict%Dictionary(1)%charvalue) + len(splitter)
    enddo
    ret_dict%Dictionary(n+1)%charvalue = line(from:)

end function
! ###########################################################

end module