module IOClass
   !! This module defines a usefull derived type for file-IO.

   !#if "INTEL"
   !    use IFPORT
   !#elif
   !#endif

   use iso_fortran_env
   use uuid_module
   use ifport
   use MathClass
   use StringClass
   use ListClass
   implicit none

   !#ifdef "INTEL"
   !    integer(int32),parameter :: STAT_ARRAY_SIZE=12
   !#elif
   !    integer(int32),parameter :: STAT_ARRAY_SIZE=13
   !#endif

   integer(int32), parameter :: PF_JSON = 1
   integer(int32), parameter :: PF_CSV = 2
   integer(int32), parameter :: PF_real64 = 1
   integer(int32), parameter :: PF_int32 = 2
   integer(int32), parameter :: PF_char = 3

   ! formats
   integer(int32), parameter :: KNT_ASCII = 100 ! K-NET ASCII

   type :: IO_
      !! This class is usefull for file-IO.
      
      integer :: fh = 100
      !! File handler
      
      logical :: active = .false.
      !! If file is opened, the active=.true.

      logical :: EOF = .true.
      !! If the reader reaches the end of file, the EOF = .true.

      character(1) :: state
      !! state of the opened file ("w"::write-only , "a"::append , or "r"::read-only.)

      character(200)::path, name, extention
      !! Triplet for identifing a file, but "name" and "extention" will be deplicated.

      character(:), allocatable:: title
      !! File title
      
      character(:), allocatable:: xlabel, ylabel, zlabel
      !! It will be deplicated.

      character(:), allocatable :: filename
      !! It will be deplicated.

      character(3) :: async = "no "
      !! For async read/write

      integer(int32) :: lastModifiedTime = 0
      !! It will be deplicated.

      logical :: json_mode = .false.
      !! Read/write a file as a JSON file. It will be deplicated.

      !> if a rule should be set, following parameter should be modified.
      logical :: activate_rule = .false.
      integer(int32) :: header, offset
      integer(int32), allocatable :: content_type(:)
      logical :: binary = .false.

   contains
      procedure, public :: unit => unitIO
      !! It returns a file handler.

      procedure, public :: numLine => numLineIO
      !! It returns a number of line in the file.

      procedure, public :: flush => flushIO
      !! It flushes all buffered content.

      procedure, public :: search_line => search_lineIO
      !! It counts the number of line in a file.

      procedure, public :: exists => existsIO
      !! If file exists, it returns .true.

      procedure, public :: cp => cpIO
      !! File copy
      
      procedure, public :: ls => ls_IO
      !! ls command

      procedure, public :: zip => zip_IOClass
      !! zip command

      procedure, public :: unzip => unzip_IOClass
      !! unzip command

      procedure, public :: download => downloadIO
      !! download a file

      
      procedure, public :: rule => ruleIO
      !! set rule

      procedure, public :: ResetRule => ResetRuleIO
      !! reset rule

      procedure, pass   :: openIOchar
      !! file open

      procedure, pass   :: openIOstring
      !! file open

      procedure, pass :: parseIOChar200
      !! parser

      procedure, pass :: parseIO2keysChar200
      !! parser

      procedure, pass :: importIOReal64ArrayAsTxt
      !! Import real64-type array from txt file

      procedure, pass :: importIOReal64VectorAsTxt
      !! Import real64-type array from txt file

      procedure, pass :: importIOReal64VectorAsTxtWithIndex
      !! Import real64-type array from txt file

      procedure, pass :: importIODataFromFormatted
      !! Import real64-type array from txt file

      !> Import real64-type array from txt file
      generic, public :: import => importIOReal64VectorAsTxt, importIOReal64ArrayAsTxt, &
         importIOReal64VectorAsTxtWithIndex, importIODataFromFormatted

      !> Exmport real64-type array from txt file
      procedure, pass :: exportIOReal64ArrayAsTxt

      !> Exmport real64-type array from txt file
      procedure, pass :: exportIOReal64VectorAsTxt

      !> Exmport real64-type array from txt file
      procedure, pass :: exportIOReal64VectorAsTxtWithIndex

      !> Exmport real64-type array from txt file
      generic, public :: export => exportIOReal64VectorAsTxt, exportIOReal64ArrayAsTxt, &
         exportIOReal64VectorAsTxtWithIndex

      !> Parser
      generic, public :: parse => parseIOChar200, parseIO2keysChar200

      !> Json parser
      procedure, pass :: parse_json_IOClass
      
      !> Json parser
      generic, public :: parse_json => parse_json_IOClass

      !> Json parser
      generic, public :: parse => parse_json_IOClass

      !> File open
      generic, public :: open => openIOchar, openIOstring

      ! file properties
      !procedure,public :: diff => diffIO
      !procedure,public :: updated => diffIO

      !> Due to stupidness of GNU and Intel,
      !> We cannot use lstat by single code without ifdef
      !> so I expired them.

      !procedure,public :: FileDateTime => FileDateTimeIO
      !procedure,public :: LastModified => FileDateTimeIO
      !procedure,public :: owner => ownerIO ! statb(5)
      !procedure,public :: size => sizeIO ! stab(8)

      ! while reading files,
      
      !> Rewind file
      procedure, public :: rewind => rewindIO

      !> Go back to some lines.
      procedure, public :: goBack => goBackIO

      !> Skip some lines.
      procedure, public :: goForward => goForwardIO

      !> Write 
      procedure, pass :: writeIOchar, writeIOcharchar, writeIOcharcharchar
      !> Write 
      procedure, pass :: writeIOstring, writeIOstringstring, writeIOstringstringstring

      !! Writer for JSON format
      procedure, pass :: dumpIOJSON_Key_Vector
      procedure, pass :: dumpIOJSON_Key_VectorRe32
      procedure, pass :: dumpIOJSON_Key_VectorInt32
      procedure, pass :: dumpIOJSON_Key_value
      procedure, pass :: dumpIOJSON_Key_valueRe32
      procedure, pass :: dumpIOJSON_Key_valueRe64
      procedure, pass :: dumpIOJSON_Key_valueChar

      procedure, pass :: dumpIOJSON_Key_ArrayRe64
      procedure, pass :: dumpIOJSON_Key_ArrayInt32

      !> JSON dump. 
      generic, public :: dump => dumpIOJSON_Key_Vector, dumpIOJSON_Key_VectorRe32, &
         dumpIOJSON_Key_VectorInt32, dumpIOJSON_Key_value, &
         dumpIOJSON_Key_valueRe32, dumpIOJSON_Key_valueRe64, &
         dumpIOJSON_Key_valueChar, dumpIOJSON_Key_ArrayRe64, &
         dumpIOJSON_Key_ArrayInt32
      ! commandline args
      procedure, public :: arg => argIO

      !! WRITE operation
      procedure, pass :: writeIOint32re64
      
      
      !! WRITE operation
      procedure, pass :: writeIOint32re64vector
      !! WRITE operation
      procedure, pass :: writeIOint32int32vector

      !! WRITE operation
      procedure, pass :: writeIOint32
      !! WRITE operation
      procedure, pass :: writeIOint32int32
      !! WRITE operation
      procedure, pass :: writeIOint32int32int32
      !! WRITE operation
      procedure, pass :: writeIOint32int32int32int32
      !! WRITE operation
      procedure, pass :: writeIOint32int32int32int32int32
      !! WRITE operation
      procedure, pass :: writeIOint32int32int32int32int32int32

      !! WRITE operation
      procedure, pass :: writeIOint32Vector
      !! WRITE operation
      procedure, pass :: writeIOint64Vector
      !! WRITE operation
      procedure, pass :: writeIOint32Vectorint32Vector
      !! WRITE operation
      procedure, pass :: writeIOint32Vectorint32Vectorint32Vector
      !! WRITE operation
      procedure, pass :: writeIOint32Vectorint32Vectorre64Vector
      !! WRITE operation
      procedure, pass :: writeIOint32Vectorre64Vector
      !! WRITE operation
      procedure, pass :: writeIOre64Vectorre64Vector
      !! WRITE operation
      procedure, pass :: writeIOre64Vectorre64Vectorre64Vector
      !! WRITE operation
      procedure, pass :: writeIOint32Array

      !! WRITE operation
      procedure, pass :: writeIOre64
      !! WRITE operation
      procedure, pass :: writeIOre64re64
      !! WRITE operation
      procedure, pass :: writeIOre64re64re64
      !! WRITE operation
      procedure, pass :: writeIOre64re64re64re64
      !! WRITE operation
      procedure, pass :: writeIOre64re64re64re64re64
      !! WRITE operation
      procedure, pass :: writeIOre64re64re64re64re64re64
      !! WRITE operation
      procedure, pass :: writeIOre64Vector
      !! WRITE operation
      procedure, pass :: writeIOre64Array
      !! WRITE operation
      procedure, pass :: writeIO_re64Vector_re64Array
      !! WRITE operation
      procedure, pass :: writeIOcomplex64
      !! WRITE operation
      procedure, pass :: writeIOcomplex64Vector
      !! WRITE operation
      procedure, pass :: writeIOcomplex64Array
      !! WRITE operation
      procedure, pass :: writeIOchar_real64Array_real64Array
      !! WRITE operation

      !> WRITE operation
      generic, public :: write => writeIOchar, writeIOstring, writeIOre64, writeIOre64Vector, &
         writeIOre64Array, &
         writeIOint32, writeIOint32Vector, writeIOint32Array, &
         writeIOre64re64, writeIOre64re64re64, writeIOre64re64re64re64, &
         writeIOre64re64re64re64re64, writeIOre64re64re64re64re64re64, &
         writeIOint32int32, writeIOint32int32int32, writeIOint32int32int32int32, &
         writeIOint32int32int32int32int32, writeIOint32int32int32int32int32int32, &
         writeIOstringstring, writeIOstringstringstring, &
         writeIOcharchar, writeIOcharcharchar, &
         writeIOint32re64vector, writeIOint32int32vector, &
         writeIOint32re64, &
         writeIOcomplex64, writeIOcomplex64Vector, writeIOcomplex64Array, &
         writeIOint32Vectorint32Vector, &
         writeIOint32Vectorre64Vector, &
         writeIOre64Vectorre64Vector, writeIOre64Vectorre64Vectorre64Vector, &
         writeIOint32Vectorint32Vectorint32Vector, &
         writeIOint32Vectorint32Vectorre64Vector, &
         writeIOchar_real64Array_real64Array, &
         writeIO_re64Vector_re64Array
      
      !! READ operation
      procedure, pass :: readIOchar

      procedure, pass :: readIOInt
      !! READ operation
      procedure, pass :: readIOIntVector
      !! READ operation
      procedure, pass :: readIOIntArray
      !! READ operation
      procedure, pass :: readIOReal64
      !! READ operation
      procedure, pass :: readIOReal64Vector
      !! READ operation
      procedure, pass :: readIOReal64VectorVector
      !! READ operation
      procedure, pass :: readIOReal64Array
      !! READ operation

      !> READ operation
      generic, public :: read => readIOchar, readIOInt, readIOIntVector, readIOIntArray &
         , readIOReal64, readIOReal64Vector, readIOReal64VectorVector, readIOReal64Array

      !> Write and plot by gnuplot.
      procedure, pass :: plotIO
      !> Write and plot by gnuplot.
      procedure, pass :: plotIODirect
      !> Write and plot by gnuplot.
      procedure, pass :: plotIODirectReal32
      !> Write and plot by gnuplot.
      generic, public :: plot => plotIO, plotIODirect, plotIODirectReal32

      !> Write and replot by gnuplot.
      procedure, pass :: replotIO
      !> Write and replot by gnuplot.
      procedure, pass :: replotIODirect
      !> Write and replot by gnuplot.
      generic, public :: replot => replotIO, replotIODirect

      !> Write and 3D-plot by gnuplot.
      procedure, public :: splot => splotIO

      !> read a line
      procedure, public :: readline => readlineIO

      !> close file
      procedure, public :: close => closeIO

      !> convert file content to real64-array
      procedure, public :: to_Array => to_Array_real64_IOClass
      !> convert file content to list
      procedure, public :: to_list => to_List_IOClass

      !> convert file content written in binary to vector
      procedure, public :: bin2vector => bin2vector_real64_IOClass
      !> convert file content written in real64 vector to binary
      procedure, public :: vector2bin => vector2bin_real64_IOClass
      !> wait for async (only for async read/write)
      procedure, public :: wait => wait_async_IOClass
   end type

   !> convert character array to vector.
   interface to_vector
      module procedure to_vector_char
   end interface

   !> fill a vector by same integer number.
   interface to_intvector
      module procedure to_intVector_int32
   end interface

   !> Capital to lowercase
   interface lowercase
      module procedure lowercaseChar, lowercaseString
   end interface

   ! print command.
   interface print
      module procedure printChar, printReal64, printComplex64, &
         printReal32, printInt64, printInt32, printInt32Int32, printCharAndIntVector
   end interface print

   ! 
   !interface disp
   !   module procedure printChar, printReal64, printReal32, printInt64, printInt32
   !end interface disp

   !> plot array by gnuplot
   interface plot
      module procedure plotRealArray
   end interface

   !> plot array by gnuplot
   interface spy
      module procedure spyRealArray
   end interface

   !> open file
   interface open_file
      module procedure open_fileIOandReturn
   end interface open_file

   !> run Caesar cipher
   interface CaesarCipher
      module procedure CaesarCipherChar
   end interface

   !> JSON to real64-array
   interface as_JSON
      module procedure as_JSONRealArray2
   end interface

   !> parse real64 array 
   interface parse
      module procedure parse_fileRealArray
   end interface

   !> export array to csv file.
   interface to_csv
      module procedure to_csv_real_array2, to_csv_real_array3, to_csv_real_vector
   end interface to_csv

   !> export array to tsv file.
   interface to_tsv
      module procedure to_tsv_real_array2, to_tsv_real_array3, to_tsv_real_vector
   end interface to_tsv

   
   !> import array from csv file.
   interface from_csv
      module procedure from_csv_real_array2, from_csv_real_array3, from_csv_real_vector, &
         from_CSV_to_vector_simple
   end interface from_csv

contains

   ! ===========================================
   subroutine readIOInt(obj, val)
      class(IO_), intent(in) :: obj
      integer(int32), intent(inout) :: val

      read (obj%fh, *) val

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOIntVector(obj, val)
      class(IO_), intent(in) :: obj
      integer(int32), intent(inout) :: val(:)

      read (obj%fh, *) val(:)

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOIntArray(obj, val)
      class(IO_), intent(in) :: obj
      integer(int32), intent(inout) :: val(:, :)

      read (obj%fh, *) val(:, :)

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOReal64(obj, val)
      class(IO_), intent(in) :: obj
      real(real64), intent(inout) :: val

      read (obj%fh, *) val

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOReal64Vector(obj, val)
      class(IO_), intent(in) :: obj
      real(real64), intent(inout) :: val(:)

      read (obj%fh, *) val(:)

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOReal64VectorVector(obj, val, val2)
      class(IO_), intent(in) :: obj
      real(real64), allocatable, intent(inout) :: val(:), val2(:)
      real(real64) :: a
      integer :: i, io

      i = 0
      do
         read (obj%fh, *, iostat=io) a
         if (io < 0) exit
         i = i + 1
      end do
      rewind (obj%fh)

      if (allocated(val)) deallocate (val)
      if (allocated(val2)) deallocate (val2)
      allocate (val(i))
      allocate (val2(i))

      do i = 1, size(val)
         read (obj%fh, *) val(i), val2(i)
      end do

   end subroutine
   ! ===========================================

   ! ===========================================
   subroutine readIOReal64Array(obj, val)
      class(IO_), intent(in) :: obj
      real(real64), intent(inout) :: val(:, :)
      integer(int32) :: i

      do i = 1, size(val, 1)
         read (obj%fh, *) val(i, 1:)
      end do

   end subroutine
   ! ===========================================

   function numLineIO(obj, name) result(line)
      class(IO_), intent(inout) :: obj
      type(IO_) :: f
      character(*), intent(in) :: name
      integer(int32) :: line
      character(len=1) :: content

      if (.not. f%exists(name)) then
         print *, "[ERROR] numLineIO >> no such file named ", name
         stop
      end if

      call f%open(name)

      line = 1
      do
         content = f%readline()
         if (f%EOF .eqv. .true.) then
            line = line - 1
            exit
         end if
         line = line + 1
      end do

      call f%close()

   end function

   ! ===========================================

   subroutine search_lineIO(obj, name, n)
      class(IO_), intent(in) :: obj
      type(IO_) :: f
      character(*), intent(in) :: name
      character(:), allocatable :: fname, command
      integer(int32), intent(inout) :: n
      integer(int32) :: system_ret
      character(len=1) :: content

      n = 0
      fname = generate_uuid() + "_search_lineIO.buffer"
      command = "sed -n '/POINT_DATA/=' "+name + " > "+fname + " "
      system_ret = system(command)

      call f%open(fname, "r")
      read (f%fh, *) n
      call f%close()
      system_ret = system("rm "+fname)

   end subroutine

   ! #############################################
   function readlineIO(obj) result(ret)
      class(IO_), intent(inout) :: obj
      character(len=:), allocatable :: ret

      if (obj%EOF .eqv. .true.) then
         print *, "ERROR :: file is not opened or EOF"
         allocate (character(len=30000) :: ret)
         ret = " "
         return
      end if

      allocate (character(len=30000) :: ret)
      read (obj%fh, '(A)', end=100) ret
      ret = trim(adjustl(ret))
      return

100   ret = " "
      obj%EOF = .true.
   end function
   ! #############################################

   ! #############################################
   function unitIO(obj) result(unit)
      class(IO_), intent(inout) :: obj
      integer(int32) :: unit
      unit = obj%fh
   end function
   ! #############################################

   ! #############################################
   recursive subroutine openIOchar(obj, path, state, name, extention, binary, async, fh)
      class(IO_), intent(inout) :: obj
      character(1), optional, intent(in) :: state ! w or r
      character(*), optional, intent(in)::path, name, extention, async
      character(:), allocatable :: localcp, async_yes_no
      integer(int32), optional, intent(in) :: fh
      integer(int32) :: tag
      logical :: yml = .False.
      logical, optional, intent(in) :: binary
      logical :: use_binary_form

      use_binary_form = .false.

      if (present(binary)) then
         use_binary_form = binary
         obj%binary = binary
      end if

      if (present(state)) then
         obj%state = state
      else
         obj%state = "w"
      end if

      if (present(async)) then
         async_yes_no = async
      else
         async_yes_no = "no"
      end if
      obj%async = async_yes_no

      !if( index(path,"mongo://")/=0  )then
      !
      !    return
      !endif

      if (index(path, "https://") /= 0 .or. index(path, "http://") /= 0) then
         ! get online file
         ! read-only
         if (present(state)) then
            if (state == "w" .or. state == "a") then
               print *, "ERROR :: OpenIOChar :: Online files are read-only."
               stop
            end if
         end if
         ! download file
         tag = index(trim(path), "/", back=.true.)
         call execute_command_line("mkdir -p ./tmp")
         call execute_command_line("wget --no-check-certificate  '"//trim(path)//"' -O ./tmp/"//trim(path(tag + 1:)))
         print *, "[ok] Downloaded > ./tmp/"//trim(path(tag + 1:))
         localcp = trim("./tmp/"//path(tag + 1:))

         call obj%open(trim(localcp), state="r", binary=binary, async=async)

         return
      end if

      !    if(present(extention) )then
      !        if( trim(extention) == "yml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".yml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".YML" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".yaml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".YAML" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == "yaml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if( index(path,"yml") /= 0 )then
      !        yml=.True.
      !    endif
      !    if( index(path,"yaml") /= 0 )then
      !        yml=.True.
      !    endif
      !
      !    if(yml .eqv. .true.)then
      !
      !        return
      !    endif

      if (obj%active .eqv. .true.) then

         print *, "ERROR :: "//trim(obj%path)//trim(obj%name)//trim(obj%extention)//" is already opened."
         stop
      end if

      obj%active = .true.
      if (present(fh)) then
         obj%fh = fh
      else
         obj%fh = 10
      end if

      obj%path = "./"
      obj%name = "untitled"
      obj%name = ".txt"

        !!! 意味不明なので削除@20240614
      !tag = index(trim(path),"/",back=.true.)
      !if(tag/=0)then
      !    call execute_command_line("mkdir -p "//path(1:tag-1) )
      !endif

      if (present(path)) then
         obj%path = trim(path)
         if (present(name)) then
            obj%name = trim(name)
            if (present(extention)) then
               obj%extention = trim(extention)
               if (present(state)) then
                  if (state == "r") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), status='old', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), status='old', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)//trim(extention)
                  elseif (state == "w") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), status='replace', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), status='replace', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)//trim(extention)
                  elseif (state == "a") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), position='append', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), position='append', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)//trim(extention)
                  else
                     call print("Error :: IOClass % open >> argument <state> should be w or r ")
                     stop
                  end if
               else
                  if (use_binary_form) then
                     open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), &
                           form="UNFORMATTED", &
                           asynchronous=async_yes_no)
                  else
                     open (newunit=obj%fh, file=trim(path)//trim(name)//trim(extention), &
                           asynchronous=async_yes_no)
                  end if
                  obj%filename = trim(path)//trim(name)//trim(extention)
               end if
            else
               if (present(state)) then
                  if (state == "r") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name), status='old', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name), status='old', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)
                  elseif (state == "w") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name), status='replace', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name), status='replace', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)
                  elseif (state == "a") then
                     if (use_binary_form) then
                        open (newunit=obj%fh, file=trim(path)//trim(name), position='append', &
                              form="UNFORMATTED", &
                              asynchronous=async_yes_no)
                     else
                        open (newunit=obj%fh, file=trim(path)//trim(name), position='append', &
                              asynchronous=async_yes_no)
                     end if
                     obj%filename = trim(path)//trim(name)
                  else
                     call print("Error :: IOClass % open >> argument <state> should be w or r ")
                     stop
                  end if
               else
                  if (use_binary_form) then
                     open (newunit=obj%fh, file=trim(path)//trim(name), &
                           form="UNFORMATTED", &
                           asynchronous=async_yes_no)
                  else
                     open (newunit=obj%fh, file=trim(path)//trim(name), &
                           asynchronous=async_yes_no)
                  end if
                  obj%filename = trim(path)//trim(name)
               end if
            end if
         else
            if (present(state)) then
               if (state == "r") then
                  if (use_binary_form) then
                     open (newunit=obj%fh, file=trim(path), status='old', &
                           form="UNFORMATTED", &
                           asynchronous=async_yes_no)
                  else
                     open (newunit=obj%fh, file=trim(path), status='old', &
                           asynchronous=async_yes_no)
                  end if
                  obj%filename = trim(path)
               elseif (state == "w") then
                  if (use_binary_form) then
                     open (newunit=obj%fh, file=trim(path), status='replace', &
                           form="UNFORMATTED", &
                           asynchronous=async_yes_no)
                  else
                     open (newunit=obj%fh, file=trim(path), status='replace', &
                           asynchronous=async_yes_no)
                  end if
                  obj%filename = trim(path)
               elseif (state == "a") then
                  if (use_binary_form) then
                     open (newunit=obj%fh, file=trim(path), position='append', &
                           form="UNFORMATTED", &
                           asynchronous=async_yes_no)
                  else
                     open (newunit=obj%fh, file=trim(path), position='append', &
                           asynchronous=async_yes_no)
                  end if

                  obj%filename = trim(path)
               else
                  call print("Error :: IOClass % open >> argument <state> should be w or r ")
                  stop
               end if
            else
               if (use_binary_form) then
                  open (newunit=obj%fh, file=trim(path), &
                        form="UNFORMATTED", &
                        asynchronous=async_yes_no)
               else
                  open (newunit=obj%fh, file=trim(path), &
                        asynchronous=async_yes_no)
               end if
               obj%filename = trim(path)
            end if
         end if
      else
         if (use_binary_form) then
            open (newunit=obj%fh, file="./untitled.txt", status="replace", &
                  form="UNFORMATTED", &
                  asynchronous=async_yes_no)
         else
            open (newunit=obj%fh, file="./untitled.txt", status="replace", &
                  asynchronous=async_yes_no)
         end if
         obj%filename = "./untitled.txt"
      end if

      obj%EOF = .false.

   end subroutine openIOchar
   ! #############################################

   ! #############################################
   subroutine openIOstring(obj, path_s, state, name_s, extention_s, binary, fh)
      class(IO_), intent(inout) :: obj
      character(1), optional, intent(in) :: state ! w or r
      type(String_), intent(in) ::path_s
      type(String_), optional, intent(in)::name_s, extention_s
      character(len=:), allocatable::path, name, extention
      integer(int32), optional, intent(in) :: fh
      logical :: yml = .False.
      logical, optional, intent(in) :: binary
      logical :: use_binary_form

      use_binary_form = .false.

      if (present(binary)) then
         use_binary_form = binary
      end if

      if (present(state)) then
         obj%state = state
      else
         obj%state = "w"
      end if
      !    if(present(extention) )then
      !        if( trim(extention) == "yml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".yml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".YML" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".yaml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == ".YAML" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if(present(extention) )then
      !        if( trim(extention) == "yaml" )then
      !            yml=.True.
      !        endif
      !    endif
      !    if( index(path,"yml") /= 0 )then
      !        yml=.True.
      !    endif
      !    if( index(path,"yaml") /= 0 )then
      !        yml=.True.
      !    endif
      !
      !    if(yml .eqv. .true.)then
      !
      !        return
      !    endif

      if (obj%active .eqv. .true.) then

         print *, "ERROR :: "//trim(obj%path)//trim(obj%name)//trim(obj%extention)//" is already opened."
         stop
      end if

      obj%active = .true.
      if (present(fh)) then
         obj%fh = fh
      else
         obj%fh = 10
      end if

      obj%path = "./"
      obj%name = "untitled"
      obj%name = ".txt"

      if (present(state)) then
         if (state == "w") then
            if (use_binary_form) then
               open (newunit=obj%fh, file=trim(path_s%str()), status="replace", &
                     form="UNFORMATTED")
            else
               open (newunit=obj%fh, file=trim(path_s%str()), status="replace")
            end if
            obj%filename = trim(path_s%str())
         elseif (state == "r") then
            if (use_binary_form) then
               open (newunit=obj%fh, file=trim(path_s%str()), status="old", &
                     form="UNFORMATTED")
            else
               open (newunit=obj%fh, file=trim(path_s%str()), status="old")
            end if
            obj%filename = trim(path_s%str())
         else
            call print("Error :: IOClass % open >> argument <state> should be w or r ")
         end if
      end if

      obj%path = trim(path_s%str())
      if (present(name_s)) then
         obj%name = trim(name_s%str())
         if (present(extention_s)) then
            obj%extention = trim(extention_s%str())
            if (use_binary_form) then
               open (newunit=obj%fh, file=trim(path_s%str())//trim(name_s%str())//trim(extention_s%str()), &
                     form="UNFORMATTED")
            else
               open (newunit=obj%fh, file=trim(path_s%str())//trim(name_s%str())//trim(extention_s%str()))
            end if
            obj%filename = trim(path_s%str())//trim(name_s%str())//trim(extention_s%str())
         else
            if (use_binary_form) then
               open (newunit=obj%fh, file=trim(path_s%str())//trim(name_s%str()), &
                     form="UNFORMATTED")
            else
               open (newunit=obj%fh, file=trim(path_s%str())//trim(name_s%str()))
            end if
            obj%filename = trim(path_s%str())//trim(name_s%str())
         end if
      else
         if (use_binary_form) then
            open (newunit=obj%fh, file=trim(path_s%str()), &
                  form="UNFORMATTED")
         else
            open (newunit=obj%fh, file=trim(path_s%str()))
         end if
         obj%filename = trim(path_s%str())
      end if

      obj%EOF = .false.

   end subroutine openIOstring
   ! #############################################

   ! #############################################
   subroutine writeIOchar(obj, char, append, advance)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: char
      logical, optional, intent(in) :: append, advance
      logical :: adv

      if (obj%binary) then
         write (obj%fh) char
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      adv = .true.
      if (present(append)) then
         adv = .not. append
      end if
      if (present(advance)) then
         adv = advance
      end if
      if (adv .eqv. .true.) then
         write (obj%fh, '(A)') char
      else
         write (obj%fh, '(A)', advance="no") char
      end if
   end subroutine writeIOchar
   ! #############################################

   ! #############################################
   subroutine writeIOchar_real64Array_real64Array(obj, char, vec1, vec2, append, advance, separator)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: char
      real(real64), intent(in) :: vec1(:), vec2(:)
      logical, optional, intent(in) :: append, advance
      logical :: adv
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) char
         write (obj%fh) vec1
         write (obj%fh) vec2
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      adv = .true.
      if (present(append)) then
         adv = .not. append
      end if
      if (present(advance)) then
         adv = advance
      end if

      if (adv .eqv. .true.) then
         do i = 1, size(vec1)
            write (obj%fh, '(A)') char + sep + str(vec1(i)) + sep + str(vec2(i))
         end do
      else
         do i = 1, size(vec1)
            write (obj%fh, '(A)', advance="no") char + sep + str(vec1(i)) + sep &
               + str(vec2(i))
         end do
      end if
   end subroutine writeIOchar_real64Array_real64Array
   ! #############################################

   ! #############################################
   subroutine writeIOcharchar(obj, char1, char2)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: char1, char2

      if (obj%binary) then
         write (obj%fh) char1
         write (obj%fh) char2
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') char1//", "//char2

   end subroutine writeIOcharchar
   ! #############################################

   ! #############################################
   subroutine writeIOcharcharchar(obj, char1, char2, char3, separator)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: char1, char2, char3
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) char1
         write (obj%fh) char2
         write (obj%fh) char3

         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') char1//sep//char2//sep//char3

   end subroutine writeIOcharcharchar
   ! #############################################

   ! #############################################
   subroutine writeIOint32(obj, in32, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32))//sep

   end subroutine writeIOint32
   ! #############################################

   ! #############################################
   subroutine writeIOint32re64(obj, in32, re64, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32
      real(real64), intent(in) :: re64
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32
         write (obj%fh) re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32))//sep//trim(str(re64))//sep

   end subroutine writeIOint32re64
   ! #############################################

   ! #############################################

   subroutine writeIOint32int32(obj, in32_1, in32_2, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32_2
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1
         write (obj%fh) in32_2
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32_1))//sep//trim(str(in32_2))//sep

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOint32int32int32(obj, in32_1, in32_2, in32_3, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32_2, in32_3
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1
         write (obj%fh) in32_2
         write (obj%fh) in32_3
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32_1))//sep// &
         trim(str(in32_2))//sep//trim(str(in32_3))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOint32int32int32int32(obj, in32_1, in32_2, in32_3, in32_4, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32_2, in32_3, in32_4
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1, in32_2, in32_3, in32_4
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32_1))//sep// &
         trim(str(in32_2))//sep//trim(str(in32_3))//sep//trim(str(in32_4))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOint32int32int32int32int32(obj, in32_1, in32_2, &
                                               in32_3, in32_4, in32_5, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32_2, in32_3, in32_4, in32_5
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1, in32_2, &
            in32_3, in32_4, in32_5
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32_1))//sep//trim(str(in32_2))//sep &
         //trim(str(in32_3))//sep//trim(str(in32_4))//sep//trim(str(in32_5))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOint32int32int32int32int32int32(obj, in32_1, &
                                                    in32_2, in32_3, in32_4, in32_5, in32_6, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32_2, in32_3, in32_4, in32_5, in32_6
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1, &
            in32_2, in32_3, in32_4, in32_5, in32_6
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(in32_1))//sep//trim(str(in32_2))//sep &
         //trim(str(in32_3))//sep//trim(str(in32_4))//sep//trim(str(in32_5)) &
         //sep//trim(str(in32_6))

   end subroutine

   ! ####################################################

   ! #############################################
   subroutine writeIOint32Vector(obj, in32, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32)
         write (obj%fh, '(A)') trim(str(in32(i)))//sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint64Vector(obj, in64, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in64(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in64)
         write (obj%fh, '(A)') trim(str(in64(i)))//sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32VectorInt32vector(obj, in32, in32_c, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:), in32_c(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32, in32_c
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32)
         write (obj%fh, '(A)') trim(str(in32(i)))//sep//trim(str(in32_c(i)))
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32VectorInt32vectorInt32Vector(obj, in32, in32_c, in32_cc, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:), in32_c(:), in32_cc(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32, in32_c, in32_cc
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32)
         write (obj%fh, '(A)') trim(str(in32(i)))//sep// &
            trim(str(in32_c(i))//sep//trim(str(in32_cc(i))))//sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32VectorInt32vectorre64Vector(obj, in32, in32_c, re64_cc, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:), in32_c(:)
      real(real64), intent(in) :: re64_cc(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32, in32_c, re64_cc
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32)
         write (obj%fh, '(A)') trim(str(in32(i)))//sep// &
            trim(str(in32_c(i))//sep//trim(str(re64_cc(i))))//sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32VectorRe64vector(obj, in32, Re64, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:)
      real(real64), intent(in) :: Re64(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32, re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32)
         write (obj%fh, '(A)') trim(str(in32(i)))//sep//trim(str(Re64(i)))//sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIORe64VectorRe64vector(obj, Re64_c, Re64, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in)  :: Re64_c(:)
      real(real64), intent(in) :: Re64(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) Re64_c, Re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(Re64_c)
         write (obj%fh, '(A)') trim(str(Re64_c(i)))//sep//trim(str(Re64(i)))
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIORe64VectorRe64vectorRe64vector(obj, Re64_cc, Re64_c, Re64, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in)  :: Re64_cc(:)
      real(real64), intent(in)  :: Re64_c(:)
      real(real64), intent(in) :: Re64(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) Re64_cc, Re64_c, Re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(Re64_c)
         write (obj%fh, '(A)') trim(str(Re64_cc(i)))//sep//trim(str(Re64_c(i)))//sep//trim(str(Re64(i)))
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32int32vector(obj, in32_1, in32, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1, in32(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1, in32
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)', advance="no") trim(str(in32_1))
      do i = 1, size(in32)
         write (obj%fh, '(A)', advance="no") sep//trim(str(in32(i)))
      end do
      write (obj%fh, '(A)', advance="yes") sep
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32re64Vector(obj, in32_1, re64, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32_1
      real(real64), intent(in) :: re64(:)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32_1, re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)', advance="no") trim(str(in32_1))
      do i = 1, size(re64)
         write (obj%fh, '(A)', advance="no") sep//trim(str(re64(i)))
      end do
      write (obj%fh, '(A)', advance="yes") sep
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOint32Array(obj, in32, separator)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: in32(:, :)
      integer(int32) :: i
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) in32
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(in32, 1)
         write (obj%fh, *) in32(i, :), sep
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOre64(obj, re64, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      write (obj%fh, '(A)') trim(str(re64))//sep

   end subroutine writeIOre64
   ! #############################################

   subroutine writeIOre64re64(obj, re64_1, re64_2, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64_1, re64_2
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64_1, re64_2
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      if (isnan(re64_1) .or. abs(re64_1) > HUGE(real64)) then
         write (obj%fh, '(A)') "NaN "//sep//trim(str(re64_2))
      elseif (isnan(re64_2) .or. abs(re64_2) > HUGE(real64)) then
         write (obj%fh, '(A)') trim(str(re64_1))//sep//"NaN"
      else
         write (obj%fh, '(A)') trim(str(re64_1))//sep//trim(str(re64_2))
      end if
   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOre64re64re64(obj, re64_1, re64_2, re64_3, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64_1, re64_2, re64_3

      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64_1, re64_2, re64_3
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(re64_1))//sep//trim(str(re64_2))//sep//trim(str(re64_3))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOre64re64re64re64(obj, re64_1, re64_2, re64_3, re64_4, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64_1, re64_2, re64_3, re64_4
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64_1, re64_2, re64_3, re64_4
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(re64_1))//sep//trim(str(re64_2))//sep//trim(str(re64_3))//sep//trim(str(re64_4))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOre64re64re64re64re64(obj, re64_1, re64_2, re64_3, re64_4, re64_5, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64_1, re64_2, re64_3, re64_4, re64_5
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64_1, re64_2, re64_3, re64_4, re64_5
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(re64_1))//sep//trim(str(re64_2))//sep &
         //trim(str(re64_3))//sep//trim(str(re64_4))//sep//trim(str(re64_5))

   end subroutine

   ! ####################################################

   ! #############################################

   subroutine writeIOre64re64re64re64re64re64(obj, re64_1, re64_2, re64_3, &
                                              re64_4, re64_5, re64_6, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64_1, re64_2, re64_3, re64_4, re64_5, re64_6
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep

      if (obj%binary) then
         write (obj%fh) re64_1, re64_2, re64_3, re64_4, re64_5, re64_6
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      write (obj%fh, '(A)') trim(str(re64_1))//sep//trim(str(re64_2))//sep &
         //trim(str(re64_3))//sep//trim(str(re64_4))//sep//trim(str(re64_5)) &
         //sep//trim(str(re64_6))

   end subroutine

   ! ####################################################

   ! #############################################
   subroutine writeIOre64Vector(obj, re64, sparse)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64(:)
      logical, optional, intent(in) :: sparse
      integer(int32) :: i

      if (obj%binary) then
         write (obj%fh) re64
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if

      if (present(sparse)) then
         if (sparse) then
            do i = 1, size(re64, 1)
               if (re64(i) == 0.0d0) then
                  write (obj%fh, '(A)', advance='no') '0 '
               else
                  write (obj%fh, '(A)', advance='no') '* '
               end if
               write (obj%fh, '(A)', advance='yes') ' '
            end do
            return
         end if
      end if

      do i = 1, size(re64)
         !write(obj%fh, '(A)') trim(str(re64(i) ))
         write (obj%fh, *) re64(i)
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOre64Array(obj, re64, sparse, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64(:, :)
      logical, optional, intent(in) :: sparse
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep
      integer(int32) :: i, j

      if (obj%binary) then
         write (obj%fh) re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if
      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      if (present(sparse)) then
         if (sparse) then

            do i = 1, size(re64, 1)
               do j = 1, size(re64, 2)
                  if (re64(i, j) == 0.0d0) then
                     write (obj%fh, '(A)', advance='no') '0 '+sep
                  else
                     write (obj%fh, '(A)', advance='no') '* '+sep
                  end if
               end do
               write (obj%fh, '(A)', advance='yes') sep
            end do
            return
         end if
      end if

      do i = 1, size(re64, 1)
         do j = 1, size(re64, 2)
            if (j /= size(re64, 2)) then
               write (obj%fh, '(A)', advance='no') str(re64(i, j)) + sep
            else
               write (obj%fh, '(A)', advance='yes') str(re64(i, j)) + sep
            end if
         end do
      end do
   end subroutine
   ! #############################################
   ! #############################################
   subroutine writeIO_re64Vector_re64Array(obj, re64v, re64, sparse, separator)
      class(IO_), intent(inout) :: obj
      real(real64), intent(in) :: re64(:, :), re64v(:)
      logical, optional, intent(in) :: sparse
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: sep
      integer(int32) :: i, j

      if (obj%binary) then
         write (obj%fh) re64v, re64
         return
      end if

      if (present(separator)) then
         sep = separator
      else
         sep = " "
      end if
      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      if (present(sparse)) then
         if (sparse) then
            do i = 1, size(re64, 1)
               write (obj%fh, '(A)', advance='no') str(re64v(i)) + sep
               do j = 1, size(re64, 2)
                  if (re64(i, j) == 0.0d0) then
                     write (obj%fh, '(A)', advance='no') '0 '+sep
                  else
                     write (obj%fh, '(A)', advance='no') '* '+sep
                  end if
               end do
               write (obj%fh, '(A)', advance='yes') sep
            end do
            return
         end if
      end if

      do i = 1, size(re64, 1)
         write (obj%fh, '(A)', advance='no') str(re64v(i)) + sep
         do j = 1, size(re64, 2)
            if (j /= size(re64, 2)) then
               write (obj%fh, '(A)', advance='no') str(re64(i, j)) + sep
            else
               write (obj%fh, '(A)', advance='yes') str(re64(i, j)) + sep
            end if
         end do
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOcomplex64(obj, complex64)
      class(IO_), intent(inout) :: obj
      complex(kind(0d0)), intent(in) :: complex64

      if (obj%binary) then
         write (obj%fh) complex64
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      write (obj%fh, '(A)') trim(str(complex64))

   end subroutine writeIOcomplex64
   ! #############################################

   ! #############################################
   subroutine writeIOcomplex64Vector(obj, complex64)
      class(IO_), intent(inout) :: obj
      complex(kind(0d0)), intent(in) :: complex64(:)
      integer(int32) :: i

      if (obj%binary) then
         write (obj%fh) complex64
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(complex64, 1)
         write (obj%fh, '(A)') trim(str(complex64(i)))
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOcomplex64Array(obj, complex64)
      class(IO_), intent(inout) :: obj
      complex(kind(0d0)), intent(in) :: complex64(:, :)
      integer(int32) :: i

      if (obj%binary) then
         write (obj%fh) complex64
         return
      end if

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      do i = 1, size(complex64, 1)
         write (obj%fh, *) complex64(i, :)
      end do
   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOstring(obj, string)
      class(IO_), intent(inout) :: obj
      type(String_), intent(in) :: string

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      write (obj%fh, '(A)') str(string)

   end subroutine writeIOstring
   ! #############################################

   ! #############################################
   subroutine writeIOstringstring(obj, string1, string2)
      class(IO_), intent(inout) :: obj
      type(String_), intent(in) :: string1, string2

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      write (obj%fh, '(A)') str(string1)//str(string2)

   end subroutine
   ! #############################################

   ! #############################################
   subroutine writeIOstringstringstring(obj, string1, string2, string3)
      class(IO_), intent(inout) :: obj
      type(String_), intent(in) :: string1, string2, string3

      if (obj%state == "r") then
         call print("IOClass >> Error >> This file is readonly. ")
         call print("Nothing is written.")
         return
      end if
      write (obj%fh, '(A)') str(string1)//" "//str(string2)//" "//str(string3)

   end subroutine
   ! #############################################

   ! #############################################
   subroutine readIOchar(obj, char)
      class(IO_), intent(inout) :: obj
      character(200) :: char

      read (obj%fh, '(A)') char

   end subroutine readIOchar
   ! #############################################

   ! #############################################
   !function readIOstring(obj) result(char)
   !    class(IO_),intent(inout) :: obj
   !    type(String_) :: char
   !
   !    read(obj%fh,'(A)' ) char%all
   !
   !end function readIOstring
   ! #############################################

   ! #############################################
   subroutine closeIO(obj, status)
      class(IO_), intent(inout) :: obj
      character(*), optional, intent(in) :: status

      if (obj%json_mode) then
         call obj%write('    "plantfem_end_signal":true')
         call obj%write("}")
         obj%json_mode = .false.
      end if

      if (obj%active .eqv. .false.) then
         print *, "ERROR :: "//"file is already closed. filename = "//obj%filename
         return
      end if
      if (present(status)) then
         close (obj%fh, status=status)
      else
         close (obj%fh)
      end if
      obj%fh = 0
      obj%active = .false.
      obj%binary = .false.

   end subroutine closeIO
   ! #############################################

   ! #############################################
   subroutine printChar(char)
      character(*), intent(in) :: char

      write (*, '(A)') trim(char)

   end subroutine
   ! #############################################

   subroutine printCharAndIntVector(char, IntVec)
      character(*), intent(in) :: char
      integer(int32), intent(in) :: IntVec(:)
      integer(int32) :: i

      call print(char)
      do i = 1, size(IntVec)
         call print(str(i)//":"//str(IntVec(i)))
      end do
   end subroutine

   ! #############################################
   subroutine printReal64(re64)
      real(real64), intent(in) :: re64
      character(20) :: char

      write (char, '(f20.10)') re64
      write (*, '(A)') trim(adjustl(char))

   end subroutine
   ! #############################################

   ! #############################################
   subroutine printComplex64(re64)
      complex(real64), intent(in) :: re64
      character(20) :: char

      write (*, *) re64

   end subroutine
   ! #############################################

   ! #############################################
   subroutine printReal32(re32)
      real(real32), intent(in) :: re32

      character(20) :: char

      write (char, '(f20.5)') re32
      write (*, '(A)') trim(adjustl(char))

   end subroutine
   ! #############################################

   ! #############################################
   subroutine printint64(in64)
      integer(int64), intent(in) :: in64

      character(40) :: char

      write (char, *) in64
      write (*, '(A)') trim(adjustl(char))

   end subroutine
   ! #############################################
   ! #############################################
   subroutine printint32(in32)
      integer(int32), intent(in) :: in32

      character(20) :: char

      write (char, '(i10)') in32
      write (*, '(A)') trim(adjustl(char))
   end subroutine
   ! #############################################

   ! #############################################
   subroutine printint32int32(in32, int32_c)
      integer(int32), intent(in) :: in32, int32_c

      character(20) :: char, char_c

      write (char, '(i10)') in32
      write (char_c, '(i10)') int32_c
      write (*, '(A)') trim(adjustl(char))//" "//trim(adjustl(char_c))
   end subroutine
   ! #############################################
   subroutine plotRealArray(x, y, z, xr, yr, zr)
      real(real64), intent(in) :: x(:), y(:)
      real(real64), optional, intent(in) :: z(:)
      character(*), optional, intent(in) :: xr, yr, zr
      type(IO_) :: f
      integer(int32) :: i

      if (present(z)) then

         call f%open("__plotRealArray__buf_.txt")
         do i = 1, size(x)
            call f%write(str(x(i))//" "//str(y(i))//" "//str(z(i)))
         end do
         call f%close()

         call print("")
         call print("Press Ctrl+c to close window.")
         call f%open("__plotRealArray__buf_.gp")
         call f%write('unset key')
         do i = 1, size(x)
            if (present(xr)) then
               call f%write('set xr'//trim(xr))
            end if
            if (present(yr)) then
               call f%write('set yr'//trim(yr))
            end if
            if (present(zr)) then
               call f%write('set zr'//trim(zr))
            end if
            call f%write('splot "__plotRealArray__buf_.txt" w l')
            call f%write("pause -1")
         end do
         call f%close()

         call execute_command_line("gnuplot __plotRealArray__buf_.gp")

         call execute_command_line("rm __plotRealArray__buf_.txt")
         call execute_command_line("rm __plotRealArray__buf_.gp")

         return
      end if

      call f%open("__plotRealArray__buf_.txt")
      do i = 1, size(x)
         call f%write(str(x(i))//" "//str(y(i)))
      end do
      call f%close()

      call print("")
      call print("Press Ctrl+c to close window.")
      call f%open("__plotRealArray__buf_.gp")
      call f%write('unset key')

      if (present(xr)) then
         call f%write('set xr'//trim(xr))
      end if
      if (present(yr)) then
         call f%write('set yr'//trim(yr))
      end if
      call f%write('plot "__plotRealArray__buf_.txt" w l')
      call f%write("pause 10")
      call f%close()

      call execute_command_line("gnuplot __plotRealArray__buf_.gp")

      call execute_command_line("rm __plotRealArray__buf_.txt")
      call execute_command_line("rm __plotRealArray__buf_.gp")

   end subroutine

   subroutine spyRealArray(array)
      real(real64), intent(in) :: array(:, :)
      real(real64), allocatable :: x(:), y(:)
      integer(int32) :: i, j, non_zero

      non_zero = 0
      do i = 1, size(array, 1)
         do j = 1, size(array, 2)
            if (array(i, j) /= 0.0d0) then
               non_zero = non_zero + 1
            end if
         end do
      end do
      allocate (x(non_zero), y(non_zero))

      non_zero = 0
      do i = 1, size(array, 1)
         do j = 1, size(array, 2)
            if (array(i, j) /= 0.0d0) then
               non_zero = non_zero + 1
               x(non_zero) = dble(i)
               y(non_zero) = dble(j)
            end if
         end do
      end do

      call plot(x=x, y=y)

   end subroutine

   ! ################################################################

   ! ################################################################
   subroutine plotIODirect(obj, x, Fx, option, logscale, name)
      class(IO_), intent(inout) ::  obj
      real(real64), intent(in) :: x(:), Fx(:)
      character(*), optional, intent(in) :: option, name
      logical, optional, intent(in) :: logscale
      character(:), allocatable :: fname

      if (present(name)) then
         fname = name
      else
         fname = "__plotIODirect__.txt"
      end if

      call obj%open(fname)
      call obj%write(x, Fx)
      call obj%close()

      call obj%plot(option=option, logscale=logscale)

   end subroutine
   ! ################################################################

   ! ################################################################
   subroutine replotIODirect(obj, x, Fx, option, logscale, name)
      class(IO_), intent(inout) ::  obj
      real(real64), intent(in) :: x(:), Fx(:)
      character(*), optional, intent(in) :: option, name
      logical, optional, intent(in) :: logscale

      character(:), allocatable :: fname

      if (present(name)) then
         fname = name
      else
         fname = "__plotIODirect__.txt"
      end if

      call obj%open(fname, "a")
      call obj%write(" ")
      call obj%write(x, Fx)
      call obj%close()

      call obj%plot(option=option, logscale=logscale)

   end subroutine
   ! ################################################################

   ! ################################################################
   subroutine plotIODirectReal32(obj, x, Fx, option, logscale, name)
      class(IO_), intent(inout) ::  obj
      real(real32), intent(in) :: x(:), Fx(:)
      character(*), optional, intent(in) :: option, name
      logical, optional, intent(in) :: logscale
      character(:), allocatable :: fname

      if (present(name)) then
         fname = name
      else
         fname = "__plotIODirect__.txt"
      end if

      call obj%open(fname, "w")
      call obj%write(dble(x), dble(Fx))
      call obj%close()

      call obj%plot(option=option, logscale=logscale)
   end subroutine
   ! ################################################################

   ! ################################################################
   subroutine plotIO(obj, name, option, logscale)
      class(IO_), intent(inout) ::  obj
      character(*), optional, intent(in) :: name
      character(*), optional, intent(in) :: option
      type(IO_) :: gp_script
      logical, optional, intent(in) :: logscale

      if (present(name)) then
         obj%filename = name
      end if
      call obj%open(obj%filename, "r")
      call gp_script%open(trim(obj%filename)//"_gp_script.gp", "w")
      call gp_script%write("set xlabel '"//obj%xlabel//"'")
      call gp_script%write("set ylabel '"//obj%ylabel//"'")
      call gp_script%write("set title '"//obj%title//"'")
      if (present(logscale)) then
         if (logscale) then
            call gp_script%write("set logscale")
         end if
      end if
      if (present(option)) then
         call gp_script%write("plot '"//obj%filename//"' "//option)
      else
         call gp_script%write("plot '"//obj%filename//"' ")
      end if

      call gp_script%close()
      call execute_command_line("gnuplot "//trim(obj%filename)//"_gp_script.gp -pause")
      call obj%close()
   end subroutine
   ! ################################################################

   ! ################################################################
   subroutine replotIO(obj, name, option, logscale)
      class(IO_), intent(inout) ::  obj
      character(*), optional, intent(in) :: name
      character(*), optional, intent(in) :: option
      type(IO_) :: gp_script
      logical, optional, intent(in) :: logscale

      if (present(name)) then
         obj%filename = name
      end if
      call obj%open(obj%filename, "r")
      call gp_script%open(trim(obj%filename)//"_gp_script.gp", "a")
      call gp_script%write("set xlabel '"//obj%xlabel//"'")
      call gp_script%write("set ylabel '"//obj%ylabel//"'")
      call gp_script%write("set title '"//obj%title//"'")
      if (present(logscale)) then
         if (logscale) then
            call gp_script%write("set logscale")
         end if
      end if
      if (present(option)) then
         call gp_script%write("plot '"//obj%filename//"' "//option)
      else
         call gp_script%write("plot '"//obj%filename//"' ")
      end if

      call gp_script%close()
      call execute_command_line("gnuplot "//trim(obj%filename)//"_gp_script.gp -pause")
      call obj%close()
   end subroutine
   ! ################################################################

   ! ################################################################
   subroutine splotIO(obj, name, option, logscale)
      class(IO_), intent(inout) ::  obj
      character(*), optional, intent(in) :: name
      character(*), optional, intent(in) :: option
      type(IO_) :: gp_script
      logical, optional, intent(in) :: logscale

      if (present(name)) then
         obj%filename = name
      end if
      call obj%open(obj%filename, "r")
      call gp_script%open(trim(obj%filename)//"_gp_script.gp", "w")
      call gp_script%write("set xlabel '"//obj%xlabel//"'")
      call gp_script%write("set ylabel '"//obj%ylabel//"'")
      call gp_script%write("set zlabel '"//obj%zlabel//"'")
      call gp_script%write("set title '"//obj%title//"'")
      if (present(logscale)) then
         if (logscale) then
            call gp_script%write("set logscale")
         end if
      end if
      if (present(option)) then
         call gp_script%write("splot '"//obj%filename//"' "//option)
      else
         call gp_script%write("splot '"//obj%filename//"' ")
      end if

      call gp_script%close()
      call execute_command_line("gnuplot "//trim(obj%filename)//"_gp_script.gp -pause")
      call obj%close()
   end subroutine

   subroutine flushIO(obj)
      class(IO_), intent(in) :: obj

      flush (obj%fh)

   end subroutine

   function parseIOChar200(obj, filename, fileformat, key1, debug) result(ret)
      class(IO_), intent(inout) :: obj
      integer(int32), optional, intent(in) :: fileformat
      character(*), intent(in) :: key1
      character(*), optional, intent(in) :: filename
      character(:), allocatable :: line
      integer(int32)::blcount = 0
      integer(int32)::rmc, id, fformat
      character(:), allocatable :: ret
      logical, optional, intent(in) :: debug
      integer(int32) :: n

      ret = ""

      fformat = input(default=1, option=fileformat)

      if (present(filename)) then
         call obj%open(filename, "r")
         if (index(filename, ".json") /= 0) then
            fformat = 1
         end if
      end if

      if (fformat == PF_JSON) then
         do
            line = obj%readline()
            if (present(debug)) then
               if (debug) then
                  print *, trim(line)
               end if
            end if
            if (adjustl(trim(line)) == "{") then
               blcount = 1
               cycle
            end if
            if (adjustl(trim(line)) == "}") then
               exit
            end if

            if (blcount == 1) then
               if (index(line, trim(key1)) /= 0) then
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if

                  id = index(line, ":")
                  ret = line(id + 1:)
                  !read(line(id+1:),*) ret
                  ret = trim(adjustl(ret))

                  call replace(ret, '"', '')
                  call replace(ret, "'", "")

                  ! [があれば]まで読む
                  if (index(ret, "[") /= 0 .and. index(ret, "]") == 0) then
                     do
                        line = obj%readline()
                        ret = trim(ret)//trim(line)
                        if (index(ret, "]") /= 0) exit
                     end do
                  end if

                  if (index(ret, "],") /= 0) then
                     ret = trim(ret)
                     ret(index(ret, "],", back=.true.) + 1:index(ret, "],", back=.true.) + 1) = " "
                  end if
                  exit
               else
                  cycle
               end if
            end if

            if (obj%EOF) exit

         end do
      end if

      if (present(filename)) then
         call obj%close()
      end if

   end function

   function parseIO2keysChar200(obj, filename, fileformat, key1, key2, debug) result(ret)
      class(IO_), intent(inout) :: obj
      integer(int32), optional, intent(in) :: fileformat
      character(*), intent(in) :: key1, key2
      character(*), optional, intent(in) :: filename
      character(:), allocatable :: line
      integer(int32)::blcount = 0
      integer(int32)::rmc, id, fformat
      character(200) :: ret
      logical, optional, intent(in) :: debug
      ret = " "

      fformat = input(default=1, option=fileformat)
      if (present(filename)) then
         call obj%open(filename, "r")
         if (index(filename, ".json") /= 0) then
            fformat = 1
         end if
      end if

      if (fformat == PF_JSON) then
         do
            line = obj%readline()

            if (present(debug)) then
               if (debug) then
                  print *, trim(line)
               end if
            end if
            if (adjustl(trim(line)) == "{") then
               blcount = 1
               cycle
            end if
            if (adjustl(trim(line)) == "}") then
               exit
            end if

            if (blcount == 1) then
               if (index(line, trim(key1)) /= 0) then
                  ! search for the second key
                  do
                     line = obj%readline()
                     if (present(debug)) then
                        if (debug) then
                           print *, trim(line)
                        end if
                     end if

                     if (index(line, "}") /= 0) then
                        exit
                     end if

                     if (index(line, trim(key2)) /= 0) then
                        rmc = index(line, ",")
                        if (rmc /= 0) then
                           line(rmc:rmc) = " "
                        end if
                        id = index(line, ":")
                        ret = line(id + 1:)
                        !read(line(id+1:),*) ret
                        ret = adjustl(ret)
                        call replace(ret, '"', '')
                        call replace(ret, "'", "")
                        ! [があれば]まで読む
                        if (index(ret, "[") /= 0 .and. index(ret, "]") == 0) then
                           do
                              line = obj%readline()
                              ret = trim(ret)//trim(line)
                              if (index(ret, "]") /= 0) exit
                           end do
                        end if
                        if (index(ret, "],") /= 0) then
                           ret(index(ret, "],", back=.true.) + 1:index(ret, "],", back=.true.) + 1) = " "
                        end if
                        exit
                     end if

                  end do
               else
                  cycle
               end if
            end if

            if (obj%EOF) exit

         end do
      end if

      if (present(filename)) then
         call obj%close()
      end if

   end function
   ! #################################################################

   ! #################################################################
   ! get last modified time
   !function FileDateTimeIO(obj,name) result(ret)
   !    class(IO_),intent(inout) :: obj
   !    character(*),optional,intent(in) :: name
   !    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)
   !
   !    if(present(name) )then
   !        call obj%open(name,"r")
   !        ierr = lstat(name, infostat)
   !        ret = infostat(10)
   !        call obj%close()
   !    else
   !        ierr = lstat(obj%filename, infostat)
   !        ret = infostat(10)
   !    endif
   !
   !end function
   ! #################################################################

   ! #################################################################
   ! get owner use id
   !function ownerIO(obj,name) result(ret)
   !    class(IO_),intent(inout) :: obj
   !    character(*),optional,intent(in) :: name
   !    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)
   !
   !    if(present(name) )then
   !        call obj%open(name,"r")
   !        ierr = lstat(name, infostat)
   !        ret = infostat(5)
   !        call obj%close()
   !    else
   !        ierr = lstat(obj%filename, infostat)
   !        ret = infostat(5)
   !    endif
   !
   !end function
   ! #################################################################

   ! #################################################################
   ! get file size
   !function sizeIO(obj,name) result(ret)
   !    class(IO_),intent(inout) :: obj
   !    character(*),optional,intent(in) :: name
   !    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)
   !
   !    if(present(name) )then
   !        call obj%open(name,"r")
   !        ierr = lstat(name, infostat)
   !        ret = infostat(8)
   !        call obj%close()
   !    else
   !        ierr = lstat(obj%filename, infostat)
   !        ret = infostat(8)
   !    endif
   !
   !end function
   ! #################################################################

   ! #################################################################
   ! detect diff
   !function diffIO(obj,name) result(ret)
   !    class(IO_),intent(inout) :: obj
   !    character(*),optional,intent(in) ::name
   !    logical :: ret
   !    integer(int32) :: lastmodified
   !    ret=.false.
   !    if(present(name) )then
   !        if( obj%lastModifiedTime == obj%LastModified( trim(name) ) )then
   !            obj%lastModifiedTime = obj%LastModified( trim(name) )
   !            ret = .false.
   !        else
   !            obj%lastModifiedTime = obj%LastModified( trim(name) )
   !            ret = .true.
   !        endif
   !    else
   !        if( obj%lastModifiedTime == obj%LastModified() )then
   !            obj%lastModifiedTime = obj%LastModified()
   !            ret = .false.
   !        else
   !            obj%lastModifiedTime = obj%LastModified()
   !            ret = .true.
   !        endif
   !    endif
   !
   !end function
   ! #################################################################

   ! #################################################################
   subroutine rewindIO(obj)
      class(IO_), intent(inout) :: obj

      rewind (obj%fh)

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine goBackIO(obj, lines)
      class(IO_), intent(inout) :: obj
      integer(int32), optional, intent(in) :: lines
      integer(int32) :: numlines, i

      numlines = input(default=1, option=lines)
      do i = 1, numlines
         backspace (obj%fh)
      end do

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine goForwardIO(obj, lines)
      class(IO_), intent(inout) :: obj
      integer(int32), optional, intent(in) :: lines
      integer(int32) :: numlines, i
      character(1):: a
      numlines = input(default=1, option=lines)
      do i = 1, numlines
         read (obj%fh, '(A)') a
      end do

   end subroutine
   ! #################################################################

   ! #################################################################
   function argIO(obj, id) result(arg)
      class(IO_), intent(in) :: obj
      integer(int32), intent(in) :: id
      character(200) :: arg
      integer(int32) :: status, length
      arg(:) = " "
      call get_command_argument(id, length=length, status=status)

      if (status == 0) then
         call get_command_argument(id, arg(1:length), status=status)
      else
         arg(:) = " "
      end if
   end function
   ! #################################################################

   ! #################################################################
   function NumberOfArgIO(obj) result(ret)
      class(IO_), intent(in) :: obj
      integer(int32) :: ret

      ret = command_argument_count()

   end function
   ! #################################################################

   ! Write for json
   ! #################################################################
   subroutine dumpIOJSON_Key_Vector(obj, key, valueVector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      real(real64), intent(in) :: valueVector(:)
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":[')
      n = size(valueVector)
      do i = 1, n - 1
         if (abs(valueVector(i)) < 1.0d0) then
            call obj%write("    0"//trim(str(valueVector(i)))//",")
         else
            call obj%write("    "//trim(str(valueVector(i)))//",")
         end if
      end do
      if (abs(valueVector(n)) < 1.0d0) then
         call obj%write("    0"//trim(str(valueVector(n))))
      else
         call obj%write("    "//trim(str(valueVector(n))))
      end if
      call obj%write("    ],")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_VectorRe32(obj, key, valueVector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      real(real32), intent(in) :: valueVector(:)
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":[')
      n = size(valueVector)
      do i = 1, n - 1
         if (abs(valueVector(i)) < 1.0d0) then
            call obj%write("    0"//trim(str(valueVector(i)))//",")
         else
            call obj%write("    "//trim(str(valueVector(i)))//",")
         end if
      end do
      if (abs(valueVector(n)) < 1.0d0) then
         call obj%write("    0"//trim(str(valueVector(n))))
      else
         call obj%write("    "//trim(str(valueVector(n))))
      end if
      call obj%write("    ],")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_VectorInt32(obj, key, valueVector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      integer(int32), intent(in) :: valueVector(:)
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":[')
      n = size(valueVector)
      do i = 1, n - 1
         call obj%write("    "//trim(str(valueVector(i)))//",")
      end do
      call obj%write("    "//trim(str(valueVector(n))))
      call obj%write("    ],")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_ArrayInt32(obj, key, valueVector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      integer(int32), intent(in) :: valueVector(:, :)
      integer(int32) :: i, n, j, m

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      n = size(valueVector, 2)
      m = size(valueVector, 1)

      call obj%write('    "'//key//'":[', advance=.false.)
      do j = 1, m - 1
         write (obj%fh, '(A)', advance="no") "["//str(valueVector(j, 1))//","

         do i = 2, n - 1
            write (obj%fh, '(A)', advance="no") " "//str(valueVector(j, i))//","
         end do
         write (obj%fh, '(A)', advance="yes") str(valueVector(j, n))//"],"

      end do
      write (obj%fh, '(A)', advance="no") "    "//"["//str(valueVector(m, 1))//","

      do i = 2, n - 1
         write (obj%fh, '(A)', advance="no") " "//str(valueVector(m, i))//","
      end do
      write (obj%fh, '(A)', advance="no") str(valueVector(m, n))//"]"
      call obj%write("],")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_ArrayRe64(obj, key, valueVector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      real(real64), intent(in) :: valueVector(:, :)
      integer(int32) :: i, n, j, m

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      n = size(valueVector, 2)
      m = size(valueVector, 1)

      call obj%write('    "'//key//'":[', advance=.false.)
      do j = 1, m - 1
         if (abs(valueVector(j, 1)) < 1.0d0) then
            write (obj%fh, '(A)', advance="no") "[0"//str(valueVector(j, 1))//","
         else
            write (obj%fh, '(A)', advance="no") "["//str(valueVector(j, 1))//","
         end if

         do i = 2, n - 1
            if (abs(valueVector(j, i)) < 1.0d0) then
               write (obj%fh, '(A)', advance="no") " 0"//str(valueVector(j, i))//","
            else
               write (obj%fh, '(A)', advance="no") " "//str(valueVector(j, i))//","
            end if
         end do
         if (abs(valueVector(j, n)) < 1.0d0) then
            write (obj%fh, '(A)', advance="yes") " 0"//str(valueVector(j, n))//"],"
         else
            write (obj%fh, '(A)', advance="yes") " "//str(valueVector(j, n))//"],"
         end if
      end do
      if (abs(valueVector(m, 1)) < 1.0d0) then
         write (obj%fh, '(A)', advance="no") "    0"//"["//str(valueVector(m, 1))//","
      else
         write (obj%fh, '(A)', advance="no") "    "//"["//str(valueVector(m, 1))//","
      end if
      do i = 2, n - 1
         if (abs(valueVector(m, i)) < 1.0d0) then
            write (obj%fh, '(A)', advance="no") " 0"//str(valueVector(m, i))//","
         else
            write (obj%fh, '(A)', advance="no") " "//str(valueVector(m, i))//","
         end if
      end do
      if (abs(valueVector(m, n)) < 1.0d0) then
         write (obj%fh, '(A)', advance="no") " 0"//str(valueVector(m, n))//"]"
      else
         write (obj%fh, '(A)', advance="no") " "//str(valueVector(m, n))//"]"
      end if
      call obj%write("],")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_value(obj, key, value)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      integer(int32), intent(in) :: value
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":'//str(value)//",")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_valueRe64(obj, key, value)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      real(real64), intent(in) :: value
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":'//str(value)//",")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_valueRe32(obj, key, value)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      real(real32), intent(in) :: value
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":'//str(value)//",")

   end subroutine
   ! #################################################################

   ! #################################################################
   subroutine dumpIOJSON_Key_valueChar(obj, key, value)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      integer(int32) :: i, n

      if (index(obj%filename, ".json") == 0) then
         print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
         return
      end if

      if (.not. obj%json_mode) then
         ! first time
         obj%json_mode = .true.
         call obj%write("{")
      end if

      call obj%write('    "'//key//'":"'//trim(value)//'",')

   end subroutine
   ! #################################################################

   ! #################################################################
   pure function lowercaseChar(line) result(ret)
      character(*), intent(in) :: line
      character(:), allocatable :: ret
      integer(int32) :: i

      ret = line
      do i = 1, len(line)
         if ("A" <= ret(i:i) .and. ret(i:i) <= "Z") then
            ret(i:i) = char(ichar(ret(i:i)) + 32)
         end if
      end do

   end function
   ! #################################################################

   ! #################################################################
   pure function lowercaseString(line) result(ret)
      type(String_), intent(in) :: line
      character(:), allocatable :: ret
      integer(int32) :: i

      ret = line%all
      do i = 1, len(line%all)
         if (ret(i:i) >= 'A' .and. ret(i:i) <= 'Z') then
            ret(i:i) = char(ichar(ret(i:i)) + 32)
         end if
      end do

   end function
   ! #################################################################

   ! #################################################################
   function CaesarCipherChar(line, fshift) result(ret)
      character(*), intent(in) :: line
      integer(int32), intent(in) :: fshift
      integer(int32) :: i
      character(:), allocatable :: ret

      ! 1-26 A-Z
      ! 27-32 [ \ ] ^ _ `
      ! 33-59 a-z
      ! 60-64 | } ~

      print *, "[Caution] bug exists"
      return
      !
      ret = line
      !do concurrent (i=1:len(line) )
      do i = 1, len(line)
         print *, cyclic(ichar(line(i:i)) + fshift, max=64)
         print *, char(i=cyclic(ichar(line(i:i)) + fshift, max=64))
         ret(i:i) = char(i=cyclic(ichar(line(i:i)) + fshift, max=64))
      end do

   end function
   ! #################################################################

   pure function cyclic(num, max) result(ret)
      integer(int32), intent(in)::num, max
      integer(int32) :: ret

      if (num > max) then
         ret = mod(num, max)
         if (ret == 0) then
            ret = max
         end if
         return
      end if

      if (num < 1) then
         ret = max + mod(num, max)
         return
      end if

      ret = num

   end function
   ! ##########################################################

   subroutine ruleIO(obj, header, offset, content_type)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: header, offset, content_type(:)

      obj%activate_rule = .true.
      obj%header = header
      obj%offset = offset
      obj%content_type = content_type

   end subroutine ruleIO
   ! ##########################################################

   subroutine ResetRuleIO(obj, header, offset, content_type)
      class(IO_), intent(inout) :: obj
      integer(int32), intent(in) :: header, offset, content_type(:)

      obj%activate_rule = .false.
      obj%header = 0
      obj%offset = 0
      if (allocated(obj%content_type)) then
         deallocate (obj%content_type)
      end if

   end subroutine ResetRuleIO
   ! ##########################################################

   ! ##########################################################
   !subroutine importIO(obj,path,realVector1,intArray)
   !
   !end subroutine
   ! ##########################################################

   function as_JSONRealArray2(real64Array, name) result(json_format_array)
      real(real64), intent(in) :: real64Array(:, :)
      character(*), intent(in) :: name
      character(:), allocatable :: json_format_array
      character(:), allocatable :: buf
      integer(int32) :: i, j

      json_format_array = "{"//name//":"
      do i = 1, size(real64Array, 1)
         buf = str(real64Array(i, 1))
         do j = 2, size(real64Array, 2)
            buf = buf//", "
            buf = buf//str(real64Array(i, 1))
         end do
         json_format_array = json_format_array//"["//buf//"]"
         if (i /= size(real64Array, 1)) then
            json_format_array = json_format_array//","
         end if
      end do
      json_format_array = json_format_array//"}"
   end function
   ! ##########################################################
   function importIOReal64VectorAsTxt(obj, name) result(real64Vector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      real(real64), allocatable :: real64Vector(:)
      integer(int32) :: i, num_lines

      ! for both comma and space
      num_lines = obj%numLine(name=name)

      allocate (real64Vector(num_lines))
      real64vector(:) = 0.0d0
      call obj%open(name, "r")
      do i = 1, num_lines
         real64Vector(i) = freal(obj%readline())
         if (obj%EOF) exit
      end do
      call obj%close()

   end function

   ! ##########################################################

   recursive function importIOReal64ArrayAsTxt(obj, name, num_column) result(real64Array)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      integer(int32), intent(in) :: num_column
      real(real64), allocatable :: real64Array(:, :)
      integer(int32) :: i, num_lines, n, m
      type(IO_) :: f
      type(List_) :: file_list

      ! for both comma and space
      if (index(name, "*") == 0) then
         num_lines = obj%numLine(name=name)

         allocate (real64Array(num_lines, num_column))
         real64Array(:, :) = 0.0d0

         call obj%open(name, "r")
         do i = 1, num_lines
            read (obj%fh, *) real64Array(i, 1:num_column)
            if (obj%EOF) exit
         end do
         call obj%close()
      else
         ! ワイルドカードにしたがってファイル一覧を取得
         file_list = f%ls(name)

         real64Array = f%import(name=file_list%get(1), num_column=num_column)

         n = size(real64Array, 1)
         m = size(real64Array, 2)*file_list%size()
         deallocate (real64Array)
         allocate (real64Array(n, m))

         do i = 1, file_list%size()
            real64Array(:, num_column*(i - 1) + 1:num_column*i) = &
               f%import(name=file_list%get(i), num_column=num_column)
         end do
      end if

   end function
   ! ##########################################################
   function importIOReal64VectorAsTxtWithIndex(obj, name, with_index) result(real64Vector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      logical, intent(in) :: with_index
      real(real64), allocatable :: real64Vector(:)
      integer(int32), allocatable :: index_list(:)
      real(real64) :: val
      integer(int32) :: i, num_lines, id, index_max, index_min

      if (.not. with_index) then
         real64Vector = obj%import(name=name)
      end if

      ! for both comma and space
      num_lines = obj%numLine(name=name)

      allocate (index_list(num_lines))
      index_list(:) = 0
      call obj%open(name, "r")
      do i = 1, num_lines
         read (obj%fh, *) index_list(i)
         if (obj%EOF) exit
      end do
      call obj%close()

      index_min = minval(index_list)
      index_max = maxval(index_list)

      allocate (real64Vector(index_min:index_max))
      real64Vector(:) = 0.0d0
      call obj%open(name, "r")
      do i = 1, num_lines
         read (obj%fh, *) id, val
         real64Vector(id) = val
         if (obj%EOF) exit
      end do
      call obj%close()

   end function

   ! ##########################################################
   subroutine exportIOReal64VectorAsTxt(obj, name, real64Vector)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      real(real64), intent(in) :: real64Vector(:)
      integer(int32) :: i

      ! for both comma and space
      call obj%open(name, "w")
      do i = 1, size(real64Vector)
         write (obj%fh, '(A)') str(real64Vector(i)) + " ,"
      end do
      call obj%close()

   end subroutine

   ! ##########################################################

   subroutine exportIOReal64ArrayAsTxt(obj, name, real64Array, num_column)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      real(real64), intent(in) :: real64Array(:, :)
      integer(int32), optional, intent(in) :: num_column
      integer(int32) :: i, num_lines, nl, j

      if (present(num_column)) then
         nl = num_column
      else
         nl = size(real64Array, 2)
      end if

      call obj%open(name, "w")
      do i = 1, size(real64Array, 1)
         do j = 1, nl - 1
            write (obj%fh, '(A)', advance='no') str(real64Array(i, j)) + " ,"
         end do
         j = nl
         write (obj%fh, '(A)', advance='yes') str(real64Array(i, j)) + " ,"
      end do
      call obj%close()
   end subroutine
   ! ##########################################################
   subroutine exportIOReal64VectorAsTxtWithIndex(obj, name, real64Vector, with_index)
      class(IO_), intent(inout) :: obj
      character(*), intent(in) :: name
      logical, intent(in) :: with_index
      real(real64), allocatable :: real64Vector(:)
      integer(int32), allocatable :: index_list(:)
      real(real64) :: val
      integer(int32) :: i, num_lines, id, index_max, index_min

      if (.not. with_index) then
         real64Vector = obj%import(name=name)
      end if

      ! for both comma and space
      call obj%open(name, "w")
      do i = 1, size(real64Vector)
         write (obj%fh, '(A)') str(i) + ","+str(real64Vector(i)) + " ,"
      end do
      call obj%close()

   end subroutine
   ! ##################################################################
   function importIODataFromFormatted(obj, format, name) result(real64Array)
      class(IO_), intent(in) :: obj
      character(*), intent(in) :: name
      integer(int32), intent(in):: format
      real(real64), allocatable :: real64Array(:, :)
      type(IO_) :: f
      character(:), allocatable :: line
      real(real64) :: sampling_Hz, duration_time, Scale_factor, t
      integer(int32) :: i, j, br1, br2, n_data, int_data(8), id

      if (format == KNT_ASCII) then
         !https://www.kyoshin.bosai.go.jp/kyoshin/man/knetform.html
         call f%open(name, "r")
         ! ignore 10 lines
         do i = 1, 10
            line = f%readline()
         end do
         line = f%readline()
         line = line(19:)
         line = line(:len(line) - 2)
         sampling_Hz = freal(line)

         line = f%readline()
         line = line(19:)
         duration_time = freal(line)

         !ignore
         line = f%readline()

         line = f%readline()
         line = line(19:)
         br1 = index(line, "(gal)/")
         br2 = index(line, "(gal)/") + 6
         Scale_factor = freal(line(1:br1 - 1))
         Scale_factor = Scale_factor/freal(line(br2:))
         n_data = int(sampling_Hz*duration_time)

         ! ignore
         do i = 1, 3
            line = f%readline()
         end do

         allocate (real64Array(n_data, 2))
         t = 0.0d0
         id = 0
         do i = 1, n_data
            if (f%EOF) exit
            line = f%readline()
            if (f%EOF) exit
            read (line, *) int_data(1:8)
            do j = 1, 8
               t = t + 1.0d0/sampling_Hz
               id = id + 1
               real64Array(id, 1) = t
               real64Array(id, 2) = dble(int_data(j))*Scale_factor
            end do
         end do

         call f%close()
      end if

   end function

   !#############################################################
   function open_fileIOandReturn(name, state) result(ret_file)
      character(1), optional, intent(in) :: state ! w or r
      character(*), intent(in) :: name
      type(IO_) :: ret_file

      call ret_file%open(name, state)

   end function

   function parse_fileRealArray(name, row, col, num_col) result(ret)
      character(*), intent(in) :: name
      integer(int32), intent(in) :: row(2), col(2)
      integer(int32), intent(in) :: num_col
      character(1), allocatable :: offset(:)
      real(real64), allocatable :: ret(:, :)
      type(IO_) :: f
      character(:), allocatable :: line
      integer(int32) :: n, m, i, row_id

      ! read
      ! row: from row(1) to row(2)
      ! col: from col(1) to col(2)
      ! as real(real64) array
      allocate (offset(col(1) - 1))
      n = row(2) - row(1) + 1
      m = num_col
      allocate (ret(n, m))
      call f%open(name, "r")
      row_id = 0
      do i = 1, row(2)
         line = f%readline()
         if (i < row(1) .or. i > row(2)) then
            cycle
         end if
         row_id = row_id + 1
         read (line(col(1):), *) ret(row_id, 1:m)
      end do
      call f%close()

   end function

   ! ######################################################
   function to_vector_char(line, num_entity) result(ret)
      character(*), intent(in) :: line
      integer(int32), optional, intent(in) :: num_entity
      character(:), allocatable :: line_buf
      real(real64), allocatable :: ret(:)
      integer(int32) :: n, s, e, i

      if (len(trim(line)) == 0) then
         allocate (ret(0))
         return
      end if

      s = index(line, "[")
      e = index(line, "]")
      line_buf = line(s + 1:e - 1)

      n = 0
      do i = 1, len(line_buf)
         if (line_buf(i:i) == ",") then
            n = n + 1
         end if
      end do
      if (present(num_entity)) then
         allocate (ret(num_entity))
      else
         allocate (ret(n + 1))
      end if
      read (line_buf, *) ret(:)

   end function
   ! ######################################################

   ! ######################################################
   function to_intVector_int32(line, num_entity) result(ret)
      character(*), intent(in) :: line
      integer(int32), optional, intent(in) :: num_entity
      character(:), allocatable :: line_buf
      integer(int32), allocatable :: ret(:)
      integer(int32) :: n, s, e, i

      line_buf = trim(line)
      if (len(trim(line_buf)) == 0) then
         allocate (ret(0))
         return
      end if

      s = index(line_buf, "[")
      e = index(line_buf, "]")
      line_buf = trim(line_buf(s + 1:e - 1))

      n = 0
      do i = 1, len(line_buf)
         if (line_buf(i:i) == ",") then
            n = n + 1
         end if
      end do
      if (present(num_entity)) then
         allocate (ret(num_entity))
      else
         allocate (ret(n + 1))
      end if
      read (line_buf, *) ret(:)

   end function
   ! ######################################################

   ! ######################################################
   subroutine to_csv_real_array3(name, a)
      real(real64), intent(in) :: a(:, :, :)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32) :: i, j, k, n1, n2, n3

      n1 = size(a, 1)
      n2 = size(a, 2)
      n3 = size(a, 3)
      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "w")
      else
         call f%open(name, "w")
      end if
      write (f%fh, *) n1, n2, n3
      do k = 1, n3
         do j = 1, n2
            do i = 1, n1
               write (f%fh, '(A)', advance="no") str(a(i, j, k)) + ","
            end do
         end do
         write (f%fh, '(A)', advance="yes") ","
      end do
      call f%close()

   end subroutine

   ! ######################################################
   subroutine to_tsv_real_array3(name, a)
      real(real64), intent(in) :: a(:, :, :)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32) :: i, j, k, n1, n2, n3

      n1 = size(a, 1)
      n2 = size(a, 2)
      n3 = size(a, 3)
      if (index(name, ".tsv") == 0) then
         call f%open(name + ".tsv", "w")
      else
         call f%open(name, "w")
      end if
      write (f%fh, *) n1, n2, n3
      do k = 1, n3
         do j = 1, n2
            do i = 1, n1
               write (f%fh, '(A)', advance="no") str(a(i, j, k)) + "    "
            end do
         end do
         write (f%fh, '(A)', advance="yes") "   "
      end do
      call f%close()

   end subroutine

! ######################################################
   subroutine to_tsv_real_array2(name, a, no_header)
      real(real64), intent(in) :: a(:, :)
      character(*), intent(in)  :: name
      integer(int32) :: i, j, k, n1, n2
      type(IO_) :: f
      logical, optional, intent(in) :: no_header
      logical :: ignore_header
      ignore_header = input(default=.true., option=no_header)

      n1 = size(a, 1)
      n2 = size(a, 2)
      if (index(name, ".tsv") == 0) then
         call f%open(name + ".tsv", "w")
      else
         call f%open(name, "w")
      end if
      if (.not. ignore_header) then
         write (f%fh, *) n1, n2
      end if
      do i = 1, n1
         do j = 1, n2 - 1
            write (f%fh, '(G31.20)', advance="no") a(i, j)
            write (f%fh, '(A)', advance="no") "    "
         end do
         j = n2
         write (f%fh, '(G31.20)', advance="no") a(i, j)
         write (f%fh, '(A)', advance="yes") "   "
      end do
      call f%close()

   end subroutine
! ###########################################

! ###########################################
   subroutine to_tsv_real_vector(name, a, no_header)
      real(real64), intent(in) :: a(:)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32) :: i, j, k, n1, n2, n3

      logical, optional, intent(in) :: no_header
      logical :: ignore_header
      ignore_header = input(default=.true., option=no_header)

      n1 = size(a, 1)
      if (index(name, ".tsv") == 0) then
         call f%open(name + ".tsv", "w")
      else
         call f%open(name, "w")
      end if

      if (.not. ignore_header) then
         write (f%fh, *) n1
      end if

      do i = 1, n1
         write (f%fh, '(A)') str(a(i)) + " "
      end do
      call f%close()

   end subroutine

! ######################################################

! ######################################################
   subroutine to_csv_real_array2(name, a, no_header)
      real(real64), intent(in) :: a(:, :)
      character(*), intent(in)  :: name
      integer(int32) :: i, j, k, n1, n2
      type(IO_) :: f
      logical, optional, intent(in) :: no_header
      logical :: ignore_header
      ignore_header = input(default=.true., option=no_header)

      n1 = size(a, 1)
      n2 = size(a, 2)
      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "w")
      else
         call f%open(name, "w")
      end if
      if (.not. ignore_header) then
         write (f%fh, *) n1, n2
      end if
      do i = 1, n1
         do j = 1, n2 - 1
            write (f%fh, '(G31.20)', advance="no") a(i, j)
            write (f%fh, '(A)', advance="no") ","
         end do
         j = n2
         write (f%fh, '(G31.20)', advance="no") a(i, j)
         write (f%fh, '(A)', advance="yes") ","
      end do
      call f%close()

   end subroutine

! ###########################################

! ###########################################
   subroutine to_csv_real_vector(name, a, no_header)
      real(real64), intent(in) :: a(:)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32) :: i, j, k, n1, n2, n3

      logical, optional, intent(in) :: no_header
      logical :: ignore_header
      ignore_header = input(default=.true., option=no_header)

      n1 = size(a, 1)
      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "w")
      else
         call f%open(name, "w")
      end if

      if (.not. ignore_header) then
         write (f%fh, *) n1
      end if

      do i = 1, n1
         write (f%fh, '(A)') str(a(i)) + ","
      end do
      call f%close()

   end subroutine

! ######################################################

   function from_csv_real_array3(name, n1, n2, n3, header) result(a)
      real(real64), allocatable :: a(:, :, :)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32), intent(in) :: n1, n2, n3
      integer(int32), optional, intent(in) :: header(1:2) ! number of line for header
      integer(int32) :: n(1:3)
      integer(int32) :: i, j, k
      character(1) :: buf

      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "r")
      else
         call f%open(name, "r")
      end if

      if (present(header)) then
         do i = header(1), header(2)
            read (f%fh, *) buf
         end do
      end if
      read (f%fh, *) n(1:3)
      allocate (a(n1, n2, n3))
      a(:, :, :) = 0.0d0
      do k = 1, n3
         read (f%fh, *) a(:, :, k)
      end do
      call f%close()

   end function
! ######################################################

   function from_csv_real_array2(name, n1, n2, header) result(a)
      real(real64), allocatable :: a(:, :)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32), intent(in) :: n1, n2
      integer(int32) :: n(1:3)
      integer(int32) :: i, j, k
      integer(int32), optional, intent(in) :: header(1:2) ! number of line for header
      character(1) :: buf
      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "r")
      else
         call f%open(name, "r")
      end if

      if (present(header)) then
         do i = header(1), header(2)
            read (f%fh, *) buf
         end do
      end if

      read (f%fh, *) n(1:2)
      allocate (a(n1, n2))
      a(:, :) = 0.0d0
      do j = 1, n2
         read (f%fh, *) a(:, j)
      end do

      call f%close()

   end function
! ######################################################
!
   function from_CSV_to_vector_simple(name, column) result(column_value)
      character(*), intent(in) ::  name
      integer(int32), intent(in) :: column
      real(real64), allocatable :: column_value(:), line(:)
      type(IO_) :: f
      integer(int32) :: n, i

      n = f%numLine(name)
      allocate (column_value(n))
      column_value = 0.0d0

      allocate (line(column))
      line = 0.0d0

      call f%open(name, "r")
      do i = 1, n
         read (f%fh, *) line(:)
         column_value(i) = line(column)
      end do
      call f%close()

   end function

! ######################################################

   function from_csv_real_vector(name, n1, header) result(a)
      real(real64), allocatable :: a(:)
      character(*), intent(in)  :: name
      type(IO_) :: f
      integer(int32), intent(in) :: n1
      integer(int32) :: n
      integer(int32), intent(in) :: header(1:2) ! number of line for header
      character(1) :: buf

      integer(int32) :: i
      if (index(name, ".csv") == 0) then
         call f%open(name + ".csv", "r")
      else
         call f%open(name, "r")
      end if

      do i = header(1), header(2)
         read (f%fh, *) buf
      end do

      read (f%fh, *) n
      allocate (a(n1))
      a(:) = 0.0d0
      do i = 1, n1
         read (f%fh, *) a(i)
      end do
      call f%close()

   end function
! ######################################################

   function existsIO(this, filename) result(exist_then_true)
      class(IO_) :: this
      character(*), intent(in) :: filename
      logical :: exist_then_true

      if (access(filename, "r") == 0) then
         exist_then_true = .true.
      else
         exist_then_true = .false.
      end if
   end function
! ######################################################

   subroutine downloadIO(this, from)
      class(IO_), intent(in) :: this
      character(*), intent(in) :: from
      integer(int32) :: system_ret

      system_ret = system("wget --no-check-certificate "+from)

   end subroutine

! #######################################################

! #####################################
   function to_Array_real64_IOClass(this, name, column, header, upsampling, debug) result(ret)
      class(IO_), intent(inout) :: this
      character(*), intent(in) :: name
      integer(int32), intent(in) :: column(:), header
      integer(int32), optional, intent(in) :: upsampling
      real(real64), allocatable :: ret(:, :), col(:), buf(:, :)
      character(200) :: charbuf
      character(:), allocatable :: line
      logical, optional, intent(in) :: debug
      integer(int32) :: i, j, k, n
      type(List_)::val_list

      if (this%active) then
         ! opened
         print *, "[ERROR] to_Array_real64_IOClass >> this should be closed."
         stop
      else

         n = this%numLine(name)

         if (.not. this%exists(name)) then
            print *, "[ERROR] numLineIO >> no such file named ", name
            stop
         end if

         allocate (ret(n - header, size(column)))
         allocate (col(maxval(column)))

         call this%open(name, "r")
         ! read header
         do i = 1, header
            read (this%fh, *) charbuf
         end do

         do i = 1, n - header
            if (present(debug)) then
               if (debug) then
                  print *, i
               end if
            end if

            if (index(name, "csv") /= 0) then
               ! read as csv file
               line = this%readline()
               !line = re(line," ","")
               call val_list%split(line, ",")
               if (maxval(column) <= val_list%size())then
                  do j = 1, size(column)
                     ret(i, j) = freal(val_list%get(column(j)))
                  end do
               endif
            else
               read (this%fh, *) col(:)
               do j = 1, size(column)
                  ret(i, j) = col(column(j))
               end do
            end if

         end do
         call this%close()

         if (present(upsampling)) then
            buf = ret
            ret = to_upsampling_ioclass(buf, upsampling=upsampling)
         end if

      end if

   end function

! #############################################################


! #####################################
   function to_list_IOClass(this, name, column, header, separator,debug) result(ret)
      class(IO_), intent(inout) :: this
      character(*),intent(in) :: separator
      character(*), intent(in) :: name
      integer(int32), intent(in) :: column, header
      real(real64), allocatable ::  col(:), buf(:, :)
      type(List_) :: ret
      character(200) :: charbuf
      character(:), allocatable :: line
      logical, optional, intent(in) :: debug
      integer(int32) :: i, j, k, n
      type(List_)::val_list

      if (this%active) then
         ! opened
         print *, "[ERROR] to_Array_real64_IOClass >> this should be closed."
         stop
      else

         n = this%numLine(name)
         call ret%new(n)

         if (.not. this%exists(name)) then
            print *, "[ERROR] numLineIO >> no such file named ", name
            stop
         end if

         call this%open(name, "r")
         ! read header
         do i = 1, header
            read (this%fh, *) charbuf
         end do

         do i = 1, n - header
            if (present(debug)) then
               if (debug) then
                  print *, i
               end if
            end if

            line = this%readline()
            !line = re(line," ","")
            call val_list%split(line, separator)
            ret%fcontent(i)%char = val_list%get(column)
            ret%fcontent(i)%char_len = len(val_list%get(column))

         end do
         call this%close()

      end if

   end function

! #############################################################

   function to_upsampling_ioclass(dataframe, upsampling) result(ret)
      real(Real64), intent(in) :: dataframe(:, :)
      integer(int32), intent(in) :: upsampling
      integer(int32) :: i, j, n
      real(real64), allocatable :: ret(:, :)

      allocate (ret(size(dataframe, 1)*upsampling, size(dataframe, 2)))
      if (upsampling == 1) then
         ret = dataframe
      elseif (upsampling == 2) then
         ! double
         n = size(dataframe, 1)

         i = 1
         ret(i*2, :) = dataframe(i, :)
         ret(i*2 - 1, :) = (dataframe(i, :) + dataframe(n, :))/2
         !$OMP parallel do
         do i = 2, size(dataframe, 1)
            ret(i*2, :) = dataframe(i, :)
            ret(i*2 - 1, :) = (dataframe(i, :) + dataframe(i - 1, :))/2
         end do
         !$OMP end parallel do
      else
         print *, "[ERROR] to_upsampling_ioclass >> NOT implemented yet."
         stop
      end if

   end function
! ######################################################
   subroutine cpIO(this, from, to)
      class(IO_) :: this
      character(*), intent(in) :: from, to
      integer(int32) :: system_ret

      system_ret = system("cp "+from + " "+to)

   end subroutine
! ######################################################

! ######################################################
   function ls_IO(this, name) result(list)
      class(IO_), intent(inout) :: this
      character(*), intent(in) :: name
      type(List_) :: list
      character(:), allocatable :: filename
      character(256) :: line
      integer(int32) :: i, n, system_ret

      filename = "lsIO_"+generate_uuid(1) + ".txt"

      system_ret = system("ls "+name + " > "+filename)
      n = this%numLine(filename)
      list = to_list()
      call this%open(filename, "r")
      do i = 1, n
         read (this%fh, *) line
         call list%append(trim(adjustl(line)))
      end do
      call this%close()
      system_ret = system("rm "+filename)

   end function
! ######################################################

! ######################################################
   subroutine extractStepResponse(name, trigger_level, buffer_size, segment_length, segment_num, dt)
      character(*), intent(in) :: name
      real(real64), intent(in) :: trigger_level
      integer(int32), intent(in) :: buffer_size
      integer(int32), intent(in) :: segment_length
      integer(int32), optional, intent(inout) :: segment_num
      real(real64), optional, intent(in) :: dt

      type(IO_) :: f
      real(real64) :: wave
      real(real64), allocatable :: buffer(:)
      real(real64) :: ave_all, ave_r, ave_l

      integer(int32) :: buffer_idx, num_line, writing_count, max_segment_length, &
                        segment_idx
      real(real64) :: switch_level
      logical :: writing_now = .false.
      type(IO_) :: output_file
      integer(int32) :: i, n

      ! setting
      switch_level = trigger_level
      max_segment_length = segment_length

      ! start >>
      num_line = f%numLine(name)

      allocate (buffer(buffer_size))
      writing_count = 0
      ! ステップ応答データを逐次読み込み，Curve fitting を行う

      call f%open(name, "r")
      buffer_idx = 0
      segment_idx = 1
      call output_file%open(name + "_"+zfill(segment_idx, 4) + ".txt", "w")
      do i_i = 1, num_line

         if (buffer_idx >= buffer_size) then
            buffer_idx = 0
            ave_r = maxval(buffer)! average(buffer(:buffer_size/4))
            ave_l = minval(buffer)!average(buffer(buffer_size-buffer_size/4+1:))
            if (abs(ave_r - ave_l) > switch_level) then
               writing_now = .true.
               writing_count = 0
            end if
         end if

         buffer_idx = buffer_idx + 1
         read (f%fh, *) buffer(buffer_idx)

         if (writing_now) then
            writing_count = writing_count + 1
            if (present(dt)) then
               write (output_file%fh, *) dt*(writing_count - 1), buffer(buffer_idx)
            else
               write (output_file%fh, *) buffer(buffer_idx)
            end if
         else
            cycle
         end if

         if (writing_count >= max_segment_length) then
            writing_count = 0
            writing_now = .false.
            call output_file%close()
            segment_idx = segment_idx + 1
            call output_file%open(name + "_"+zfill(segment_idx, 4) + ".txt", "w")
         end if
      end do

      call f%close()
      if (present(segment_num)) then
         segment_num = segment_idx - 1
      end if

   end subroutine
! #####################################################################

! #####################################################################
   subroutine wait_async_IOClass(this)
      class(IO_), intent(in) :: this

      if ((index(this%async, "Y") /= 0) .or. (index(this%async, "y") /= 0)) then
         wait(this%fh)
      end if

   end subroutine
! #####################################################################

! #####################################################################
   subroutine zip_IOClass(this, zipfile, filename)
      class(IO_), intent(in) :: this
      character(*), intent(in) :: zipfile, filename
      integer(int32) :: system_ret

      system_ret = system("zip -m "+zipfile + " "+filename)

   end subroutine
! #####################################################################

! #####################################################################
   subroutine unzip_IOClass(this, zipfile, filename)
      class(IO_), intent(in) :: this
      character(*), intent(in) :: zipfile, filename
      integer(int32) :: system_ret

      if (filename == "*" .or. trim(filename) == "") then
         system_ret = system("unzip -u "+zipfile)
      else
         system_ret = system("unzip -u "+zipfile + " "+filename)
      end if

   end subroutine
! #####################################################################

! #####################################################################
   subroutine vector2bin_real64_IOClass(this, name, vec)
      class(IO_), intent(in) :: this
      character(*), intent(in) :: name
      type(IO_) :: f
      real(real64), intent(in) :: vec(:)

      call f%open(name, "w", binary=.true.)
      write (f%fh) size(vec)
      write (f%fh) vec(:)
      call f%close()

   end subroutine
! #####################################################################

! #####################################################################
   subroutine bin2vector_real64_IOClass(this, name, vec)
      class(IO_), intent(in) :: this
      character(*), intent(in) :: name
      type(IO_) :: f
      real(real64), allocatable, intent(inout) :: vec(:)
      integer(int32) :: n
      logical :: original_binary

      call f%open(name, "r", binary=.true.)
      read (f%fh) n

      if (allocated(vec)) then
         if (size(vec) == n) then
            ! do nothing
         else
            deallocate (vec)
            allocate (vec(n))
         end if
      else
         allocate (vec(n))
      end if
      read (f%fh) vec(:)
      call f%close()

   end subroutine
! #####################################################################

! #####################################################################
   recursive function parse_json_IOClass(this, filename, keys, from_line_idx) result(ret)
      class(IO_), intent(inout) :: this
      type(IO_) :: f, debug, g
      character(*), intent(in) :: filename
      type(List_), intent(in) :: keys
      type(List_) :: mini_keys
      integer(int32), optional, intent(in) :: from_line_idx
      integer(int32) :: line_idx, key_idx, num_bracket, num_line
      character(:), allocatable :: line, ret, not_found

      not_found = "{'not found'}"
      ret = not_found

      line_idx = 0

      call g%open(filename, "r")

      if (present(from_line_idx)) then
         do line_idx = 1, from_line_idx - 1
            line = g%readline()
            not_found = "{'not found loop'}"
         end do
      end if

      do
         line_idx = line_idx + 1
         if (g%EOF) exit
         line = g%readline()
         if (g%EOF) exit

         if (keys%get(1) .in.line) then
            if (size(keys%fcontent) == 1) then
               if ("[".in.line) then
                  num_bracket = 1
                  ! import array
                  ret = line
                  do
                     line_idx = line_idx + 1
                     line = g%readline()
                     if ("[".in.line) then
                        num_bracket = num_bracket + 1
                     end if
                     if ("]".in.line) then
                        num_bracket = num_bracket - 1
                     end if
                     ret = ret//line
                     if (num_bracket == 0) then
                        exit
                     end if
                  end do
                  call replace(ret, keys%get(1), "")
                  call replace(ret, '"', "")
                  call replace(ret, "'", "")
                  call replace(ret, ":", "")
                  call replace(ret, "],", "]")
                  call replace(ret, "][", "],[")
                  close (g%fh)
                  return
               else
                  ret = line
                  call replace(ret, keys%get(1), "")
                  call replace(ret, '"', "")
                  call replace(ret, "'", "")
                  call replace(ret, ":", "")
                  call replace(ret, ",", "")
                  close (g%fh)
                  return
               end if
            end if
         end if

         if (keys%get(1) .in.line) then
            mini_keys%fcontent = keys%fcontent(2:)
            ret = f%parse_json(filename=filename, keys=mini_keys, from_line_idx=line_idx)
            close (g%fh)
            return
         end if
      end do
      close (g%fh)

   end function
! #####################################################################

end module IOClass
