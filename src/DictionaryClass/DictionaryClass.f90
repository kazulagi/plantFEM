module DictionaryClass
    use MathClass

    type ::  Page_
        character*200 :: value
    end type

    type :: Dictionary_
        type(Page_),allocatable :: Dictionary(:)
    contains
        procedure :: Init => InitializeDictionary
        procedure :: Input => InputDictionary
        procedure :: Get => GetDictionaryValue
        procedure :: GetPageNum => GetPageNumDictionary 
        procedure :: show => showDictionary

    end type

    type, extends(Page_) :: FileInfo_
        character*200 :: Path
        character*200 :: DirectoryName
        character*200 :: FileName
        integer       :: FileID
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
    integer,intent(in)      :: NumOfPage
    if(allocated(obj%Dictionary) )then
        deallocate(obj%Dictionary)
    endif

    allocate(obj%Dictionary(NumOfPage) )

end subroutine
! ##############################################


! ##############################################
subroutine InputDictionary(obj,page,content)
    class(Dictionary_),intent(inout)::obj
    integer,intent(in)      :: page
    character(*),intent(in)           :: content

    if(page > size(obj%Dictionary) )then
        print *, "Error :: InputDictionary >> Num of Page is overflowed"
        stop
    endif
    obj%Dictionary(page)%Value = content 

end subroutine
! ##############################################

! ##############################################
function GetDictionaryValue(obj,page) result(content)
    class(Dictionary_),intent(in)::obj
    integer,intent(in)      :: page
    character*200 :: content

    content = obj%Dictionary(page)%Value

end function
! ##############################################

! ##############################################
subroutine setFilePath(obj,FilePath,FileID) 
    class(FileList_),intent(inout)::obj
    integer,intent(in) :: FileID
    character*200,intent(in) :: FilePath

    obj%FileList(FileID)%Path = FilePath

end subroutine
! ##############################################

! ##############################################
subroutine setDirectoryName(obj,DirectoryName,FileID) 
    class(FileList_),intent(inout)::obj
    integer,intent(in) :: FileID
    character*200,intent(in) :: DirectoryName

    obj%FileList(FileID)%DirectoryName = DirectoryName

end subroutine
! ##############################################

! ##############################################
subroutine setFileName(obj,FileName,FileID) 
    class(FileList_),intent(inout)::obj
    integer,intent(in) :: FileID
    character*200,intent(in) :: FileName

    obj%FileList(FileID)%FileName = FileName

end subroutine
! ##############################################


! ##############################################
subroutine showDictionary(obj,From,to)
    class(Dictionary_)::obj
    integer,optional,intent(in)::From,to
    integer :: i,n,startp,endp

    n=size(obj%Dictionary,1)

    startp=input(default=1,option=From)
    endp  =input(default=n,option=to)
    do i=startp,endp
        print *, "Page : ",i,"Content : ",trim(obj%Dictionary(i)%Value )
    enddo

end subroutine
! ##############################################


! ##############################################
function GetPageNumDictionary(obj,Content) result(page)
    class(Dictionary_),intent(in)::obj
    character(*),intent(in)::Content
    integer :: page
    integer :: i,n

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