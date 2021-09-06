module DictionaryClass
    use, intrinsic :: iso_fortran_env
    use MathClass

    implicit none

    type ::  Page_
        character*200 :: value
        integer(int32) :: IntValue
        real(real64) :: RealValue
        integer(int32),allocatable :: intlist(:)
        real(real64),allocatable :: realist(:)
    end type

    type :: Dictionary_
        type(Page_),allocatable :: Dictionary(:)
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

contains

! ##############################################
subroutine InitializeDictionary(obj,NumOfPage)
    class(Dictionary_),intent(inout)::obj
    integer(int32),intent(in)      :: NumOfPage
    if(allocated(obj%Dictionary) )then
        deallocate(obj%Dictionary)
    endif
    allocate(obj%Dictionary(NumOfPage) )

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
        obj%Dictionary(page)%Value = content 
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
    character*200 :: content

    content = obj%Dictionary(page)%Value

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

    n=size(obj%Dictionary,1)

    
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
    
        print *, "Page : ",i,"Content : ",trim(obj%Dictionary(i)%Value ),&
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
    

    if(present(Name) )then
        open(1023,file=trim(Name))
        
        
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
            write(1023,*) "Page : ",i,"Content : ",trim(obj%Dictionary(i)%Value ),&
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
    open(nnn,file=trim(FileName))
    
    
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
        write(nnn,*) "Page : ",i,"Content : ",trim(obj%Dictionary(i)%Value ),&
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
 
    content = obj%Dictionary(id)%value

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
        if(trim(Content)==trim(obj%Dictionary(i)%value) )then
            page=i
            return
        endif
    enddo
    if(page==-1)then
        print *, "ERROR ::",trim(Content)," is a word to be found only in the dictionary of fools."
    endif



end function
! ##############################################



end module