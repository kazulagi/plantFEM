module PreprocessingClass
    use mpiclass
    use termclass
    use FEMDomainClass
    use PostProcessingClass
    

    implicit none    
    
    type :: PreProcessing_
        type(FEMDomain_) :: FEMDomain
        character*200   :: PictureName
        integer         :: PixcelSize(2)
        integer         :: ColorRGB(3)
    contains
        procedure :: Init               => InitializePrePro
        procedure :: ImportPictureName  => ImportPictureName
        procedure :: ShowName           => ShowPictureName
        procedure :: ShowPixcelSize     => ShowPixcelSize
        procedure :: GetPixcelSize      => GetPixcelSize 
        procedure :: GetAllPointCloud   => GetAllPointCloud
        procedure :: SetColor           => SetColor
        procedure :: ShowColor          => ShowColor
        procedure :: GetPixcelByRGB     => GetPixcelByRGB
        procedure :: GetSurfaceNode     => GetPixcelSurfaceNode
        procedure :: AssembleSurfaceElement => AssembleSurfaceElement
        procedure :: ReduceSize         => ReduceSize
        procedure :: ExportGeoFile      => ExportGeoFile
        procedure :: ConvertGeo2Msh     => ConvertGeo2Msh
        procedure :: ConvertGeo2Inp     => ConvertGeo2Inp
        procedure :: ConvertGeo2Mesh    => ConvertGeo2Mesh
        procedure :: ConvertMsh2Scf     => ConvertMsh2Scf
        procedure :: ConvertMesh2Scf    => ConvertMesh2Scf
        procedure :: Export             => ExportPreProcessing
        procedure :: Reverse            => ReversePreProcessing
        procedure :: SetDataType        => SetDataTypeFEMDomain
        procedure :: SetSolver          => SetSolverPreProcessing 
        procedure :: SetUp              => SetUpPreprocessing
        procedure :: SetScale           => SetScalePreProcessing
        procedure :: SetBC              => SetBoundaryConditionPrePro
        procedure :: SetSizeOfBC        => SetSizeOfBCPrePrecessing
        procedure :: SetMatPara         => SetMatParaPreProcessing
        procedure :: SetMatID           => SetMatIDPreProcessing
        procedure :: ShowBC             => ShowBCPrePrecessing
        procedure :: Convert3Dto2D      => Convert3Dto2D
        procedure :: Convert2Dto3D      => Convert2Dto3D
        procedure :: SetControlPara     => SetControlParaPrePro
        procedure :: getSkelton         => getSkeltonPreProcessing
        procedure :: Boolean            => BooleanModifyerPreProcessing
        procedure :: setEntity          => setEntityPreProcessing
        procedure :: showMesh           => showMeshPreProcessing 
    end type
    
contains

! #########################################################
subroutine InitializePrePro(obj,Default)
    class(PreProcessing_),intent(inout)::obj
    logical,optional,intent(in)::Default

    call obj%FEMDomain%Init(Default)
end subroutine
! #########################################################


! #########################################################
subroutine ImportPictureName(obj,InPictureName)
    class(PreProcessing_)::obj
    character*200,intent(in)::InPictureName
    obj%PictureName=trim(InPictureName)
end subroutine
! #########################################################

! #########################################################
subroutine ShowPictureName(obj)
    class(PreProcessing_)::obj
    print *, trim(obj%PictureName)
end subroutine 
! #########################################################


! #########################################################
subroutine ShowPixcelSize(obj)
    class(PreProcessing_)::obj
    character *20       :: pix_x
    character *20       :: pix_y
    write(pix_x,*) obj%PixcelSize(1)
    write(pix_y,*) obj%PixcelSize(2)
    
    print *, "Pixcel size is :: ",trim(adjustl(pix_x) )," x ",&
    trim(adjustl(pix_y) )
end subroutine 
! #########################################################



! #########################################################
subroutine GetPixcelSize(obj,MPIData,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),optional,intent(inout)          :: MPIData
    character(*),optional,intent(in)   :: Name
    character *20       :: pid
    character *200      :: python_script
    character *200      :: python_buffer
    character *200      :: command
    integer              :: fh

    call MPIData%GetInfo()
    write(pid,*) MPIData%MyRank
    python_script="GetPixcelSize_pid_"//trim(adjustl(pid))//".py"
    python_buffer="GetPixcelSize_pid_"//trim(adjustl(pid))//".txt"
    if(present(Name) )then
        python_script=Name//"GetPixcelSize_pid_"//trim(adjustl(pid))//".py"
        python_buffer=Name//"GetPixcelSize_pid_"//trim(adjustl(pid))//".txt"
    endif

    !print *, trim(python_script)

    ! using python script
    ! python imaging library is to be installed.
    fh=MPIData%MyRank+10
    open(fh,file=trim(python_script),status="replace")
    command = "from PIL import Image"
    write(fh,'(A)') adjustl(trim(command))
    command = "import sys"
    write(fh,'(A)') adjustl(trim(command))
    command = "import os"
    write(fh,'(A)') adjustl(trim(command))

    ! open file
    command = 'img_in = Image.open("'//trim(obj%PictureName)//'")'
    write(fh,'(A)') adjustl(trim(command))
    command = 'python_buffer = open("'//trim(python_buffer)//'","w")'
    write(fh,'(A)') adjustl(trim(command))

    ! get pixcel size
    command = "rgb_im = img_in.convert('RGB')"
    write(fh,'(A)') adjustl(trim(command))
    command = "size = rgb_im.size"
    write(fh,'(A)') adjustl(trim(command))
    command = "print( str(size[0]), ' ',str(size[1])  ) "
    write(fh,'(A)') adjustl(trim(command))
    
    ! write size
    command = "python_buffer.write( str(size[0]))"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer.write('\n')"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer.write( str(size[1]))"
    write(fh,'(A)') adjustl(trim(command))
    
    ! close
    command = "img_in.close()"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer.close()"
    write(fh,'(A)') adjustl(trim(command))
    close(fh)

    command = "python "//trim(python_script)
    print *, trim(command)
    call execute_command_line(trim(command))

    ! get pixcel size
    open(fh,file=trim(python_buffer),status="old")
    read(fh,*) obj%PixcelSize(1)
    read(fh,*) obj%PixcelSize(2)
    close(fh)


end subroutine
! #########################################################


! #########################################################
subroutine GetAllPointCloud(obj,MPIData,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in)   :: Name
    character *20       :: pid
    character *200      :: python_script
    character *200      :: python_buffer
    character *200      :: command
    integer              :: fh

    call MPIData%GetInfo()
    write(pid,*) MPIData%MyRank
    
    python_script="GetAllPointCloud_pid_"//trim(adjustl(pid))//".py"
    python_buffer="GetAllPointCloud_pid_"//trim(adjustl(pid))//".txt"
    if(present(Name) )then
        python_script=Name//"GetAllPointCloud_pid_"//trim(adjustl(pid))//".py"
        python_buffer=Name//"GetAllPointCloud_pid_"//trim(adjustl(pid))//".txt"
    endif

    print *, trim(python_script)

    ! using python script
    ! python imaging library is to be installed.
    fh=MPIData%MyRank+10
    open(fh,file=trim(python_script),status="replace")
    command = "from PIL import Image"
    write(fh,'(A)') adjustl(trim(command))
    command = "import sys"
    write(fh,'(A)') adjustl(trim(command))
    command = "import os"
    write(fh,'(A)') adjustl(trim(command))

    ! open file
    command = 'img_in = Image.open("'//trim(obj%PictureName)//'")'
    write(fh,'(A)') adjustl(trim(command))
    command = 'python_buffer = open("'//trim(python_buffer)//'","w")'
    write(fh,'(A)') adjustl(trim(command))

    ! get pixcel size
    command = "rgb_im = img_in.convert('RGB')"
    write(fh,'(A)') adjustl(trim(command))
    command = "size = rgb_im.size"
    write(fh,'(A)') adjustl(trim(command))
    command = "print( str(size[0]), ' ',str(size[1])  ) "
    write(fh,'(A)') adjustl(trim(command))
    
    ! get rgb pixcel coordinates
    command = "width,height =img_in.size"
    write(fh,'(A)') adjustl(trim(command))
    command = "for i in range(width):"
    write(fh,'(A)') adjustl(trim(command))
    command = "for j in range(height):"
    write(fh,'(A)') "   "//adjustl(trim(command))
    command = "R,G,B=rgb_im.getpixel((i,j))"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "python_buffer.write(str(i)+'\t')"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "python_buffer.write(str(j)+'\t')"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "python_buffer.write(str(R)+'\t')"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "python_buffer.write(str(G)+'\t')"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "python_buffer.write(str(B)+'\n')"
    write(fh,'(A)') "       "//adjustl(trim(command))
    
    ! close
    command = "img_in.close()"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer.close()"
    write(fh,'(A)') adjustl(trim(command))
    close(fh)

    command = "python "//trim(python_script)
    print *, trim(command)
    call execute_command_line(trim(command))

end subroutine
! #########################################################

! #########################################################
subroutine GetPixcelByRGB(obj,MPIData,err,onlycoord,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    integer,optional,intent(in)        :: err
    logical,optional,intent(in)        :: onlycoord
    character(*),optional,intent(in)   :: Name
    character *20       :: pid
    character *20       :: Red,Green,Blue
    character *20       :: er
    character *200      :: python_script
    character *200      :: python_buffer
    character *200      :: python_buffer_size
    character *200      :: command
    integer              :: fh,error,sizeofpc,i

    if(present(err) )then
        error=err
    else
        error=0
    endif

    call MPIData%GetInfo()
    write(pid,*) MPIData%MyRank
    write(Red,*) obj%ColorRGB(1)
    write(Green,*) obj%ColorRGB(2)
    write(Blue,*) obj%ColorRGB(3)
    write(er,*) error

    python_script="GetPixcelByRGB_pid_"//trim(adjustl(pid))//".py"
    python_buffer="GetPixcelByRGB_pid_"//trim(adjustl(pid))//".txt"
    python_buffer_size="GetPixcelByRGB_size_pid_"//trim(adjustl(pid))//".txt"

    if(present(Name) )then
        python_script       =Name//"GetPixcelByRGB_pid_"//trim(adjustl(pid))//".py"
        python_buffer       =Name//"GetPixcelByRGB_pid_"//trim(adjustl(pid))//".txt"
        python_buffer_size  =Name//"GetPixcelByRGB_size_pid_"//trim(adjustl(pid))//".txt"
    endif
    print *, trim(python_script)

    ! using python script
    ! python imaging library is to be installed.
    fh=MPIData%MyRank+10
    open(fh,file=trim(python_script),status="replace")
    command = "from PIL import Image"
    write(fh,'(A)') adjustl(trim(command))
    command = "import sys"
    write(fh,'(A)') adjustl(trim(command))
    command = "import os"
    write(fh,'(A)') adjustl(trim(command))

    ! open file
    command = 'img_in = Image.open("'//trim(obj%PictureName)//'")'
    write(fh,'(A)') adjustl(trim(command))
    command = 'python_buffer = open("'//trim(python_buffer)//'","w")'
    write(fh,'(A)') adjustl(trim(command))
    command = 'python_buffer_size = open("'//trim(python_buffer_size)//'","w")'
    write(fh,'(A)') adjustl(trim(command))

    ! get pixcel size
    command = "rgb_im = img_in.convert('RGB')"
    write(fh,'(A)') adjustl(trim(command))
    command = "size = rgb_im.size"
    write(fh,'(A)') adjustl(trim(command))
    command = "print( str(size[0]), ' ',str(size[1])  ) "
    write(fh,'(A)') adjustl(trim(command))
    
    ! get rgb pixcel coordinates

    command = "itr = 0"
    write(fh,'(A)') adjustl(trim(command))

    command = "width,height =img_in.size"
    write(fh,'(A)') adjustl(trim(command))
    command = "for i in range(width):"
    write(fh,'(A)') adjustl(trim(command))
    command = "for j in range(height):"
    write(fh,'(A)') "   "//adjustl(trim(command))
    command = "R,G,B=rgb_im.getpixel((i,j))"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "er=abs(R-"//adjustl(trim(Red))//&
        ")+abs(G-"//adjustl(trim(Green))//&
        ")+abs(B-"//adjustl(trim(Blue))//")"
    write(fh,'(A)') "       "//adjustl(trim(command))
    command = "if er <= "//adjustl(trim(er))// " :"
    write(fh,'(A)') "       "//adjustl(trim(command))

    command = "python_buffer.write(str(i)+'\t')"
    write(fh,'(A)') "           "//adjustl(trim(command))
    command = "itr=itr+1"
    write(fh,'(A)') "           "//adjustl(trim(command))
    if( onlycoord .eqv. .true. )then
        command = "python_buffer.write(str(j)+'\n')"
        write(fh,'(A)') "           "//adjustl(trim(command))
    else
        command = "python_buffer.write(str(j)+'\t')"
        write(fh,'(A)') "           "//adjustl(trim(command))
        command = "python_buffer.write(str(R)+'\t')"
        write(fh,'(A)') "           "//adjustl(trim(command))
        command = "python_buffer.write(str(G)+'\t')"
        write(fh,'(A)') "           "//adjustl(trim(command))
        command = "python_buffer.write(str(B)+'\n')"
        write(fh,'(A)') "           "//adjustl(trim(command))
    endif
    
    ! close
    command = "python_buffer_size.write(str(itr)+'\n')"
    write(fh,'(A)') adjustl(trim(command))
    command = "img_in.close()"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer.close()"
    write(fh,'(A)') adjustl(trim(command))
    command = "python_buffer_size.close()"
    write(fh,'(A)') adjustl(trim(command))
    close(fh)

    command = "python "//trim(python_script)
    print *, trim(command)
    call execute_command_line(trim(command))


    open(fh,file=python_buffer_size,status="old")
    read(fh,*) sizeofpc
    close(fh)

    allocate(obj%FEMDomain%Mesh%NodCoord(sizeofpc,3) )
    obj%FEMDomain%Mesh%NodCoord(:,3)=0.0d0
    open(fh,file=python_buffer,status="old")
    do i=1,sizeofpc
        read(fh,*)obj%FEMDomain%Mesh%NodCoord(i,1:2)
    enddo
    obj%FEMDomain%Mesh%NodCoord(i,2)=-1.0d0*obj%FEMDomain%Mesh%NodCoord(i,2)
    close(fh)


end subroutine
! #########################################################


! #########################################################
subroutine SetColor(obj,Red,Green,Blue)
    class(PreProcessing_),intent(inout):: obj
    integer,intent(in) :: Red,Green,Blue

    obj%ColorRGB(1)=Red
    obj%ColorRGB(2)=Green
    obj%ColorRGB(3)=Blue

end subroutine
! #########################################################

! #########################################################
subroutine ShowColor(obj)
    class(PreProcessing_),intent(inout):: obj
    
    print *, "Object Name is : ",trim(obj%PictureName)
    print *, "Red   : ",obj%ColorRGB(1)
    print *, "Green : ",obj%ColorRGB(2)
    print *, "Blue  : ",obj%ColorRGB(3)
    
end subroutine
! #########################################################


! #########################################################
subroutine GetPixcelSurfaceNode(obj,MPIData,r,NumOfMaxNod,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in)   :: Name
    integer,optional,intent(in)        :: r,NumOfMaxNod
    character*200   :: python_buffer
    character*20    :: pid
    
    integer,allocatable :: KilledPixcel(:)
    integer :: i,j,n,node_id,check_node_id,point_count,fh,MaxNod
    real(8) :: x_real,y_real,z_real
    real(8) :: x_real_tr,y_real_tr,z_real_tr,diff_real,max_r
    real(8),allocatable :: buffer(:,:)

    if(present(r) )then
        max_r=r
    else
        max_r=sqrt(2.10d0)
    endif

    if(present(NumOfMaxNod) )then
        MaxNod=NumOfMaxNod
    else
        MaxNod=7
    endif

    n=size(obj%FEMDomain%Mesh%NodCoord,1)
    allocate(KilledPixcel(n) )
    KilledPixcel(:)=0
    
    ! remove isolated pixcel and surrounded nodes
    do i=1,n
        point_count=0
        do j=1,n

            check_node_id=j
            x_real=obj%FEMDomain%Mesh%NodCoord(i,1)
            y_real=obj%FEMDomain%Mesh%NodCoord(i,2)
            z_real=obj%FEMDomain%Mesh%NodCoord(i,3)
            x_real_tr=obj%FEMDomain%Mesh%NodCoord(j,1)
            y_real_tr=obj%FEMDomain%Mesh%NodCoord(j,2)
            z_real_tr=obj%FEMDomain%Mesh%NodCoord(j,3)
            
            diff_real=(x_real-x_real_tr)*(x_real-x_real_tr)+&
                (y_real-y_real_tr)*(y_real-y_real_tr)+&
                (z_real-z_real_tr)*(z_real-z_real_tr)

            diff_real=dsqrt(diff_real)
            

            if(diff_real < max_r)then
                point_count=point_count+1
            endif

            if(point_count > MaxNod )then
                KilledPixcel(i)=1
                exit
            endif
        enddo
        if(point_count ==0 )then
            KilledPixcel(i)=1
        endif
    enddo



    call MPIData%GetInfo()
    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".txt"
    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".txt"    
    endif

    open(fh,file=python_buffer,status="replace")
    do i=1,n
        if(KilledPixcel(i)==0 )then
            write(fh,*) obj%FEMDomain%Mesh%NodCoord(i,1:2)
        else
            cycle
        endif
    enddo
    close(fh)

    do i=1,n
        point_count = point_count + KilledPixcel(i)
    enddo
    allocate(buffer(n-point_count,3) )
    point_count=0
    do i=1,n
        if(KilledPixcel(i)==0)then
            point_count = point_count + 1
            buffer(point_count,:)=obj%FEMDomain%Mesh%NodCoord(i,:)
        else
            cycle
        endif
    enddo
    deallocate(obj%FEMDomain%Mesh%NodCoord)
    allocate(obj%FEMDomain%Mesh%NodCoord(size(buffer,1),size(buffer,2) ) )
    obj%FEMDomain%Mesh%NodCoord(:,:)=buffer(:,:)
    



end subroutine
! #########################################################



! #########################################################
subroutine AssembleSurfaceElement(obj,MPIData,dim,threshold,DelRange,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    integer,optional,intent(in)        :: dim,threshold,DelRange
    character(*),optional,intent(in)   :: Name
    character*200   :: python_buffer
    character*20    :: pid
    real(8),allocatable     :: buffer(:,:),r_value(:)
    integer,allocatable     :: checked(:)
    real(8)                 :: x(3),x_tr(3),r_tr,r_ref
    integer :: i,j,k,n,trial_num,id_tr,fh,r_threshold,drange

    if(present(threshold) )then
        r_threshold=threshold
    else
        r_threshold=10
    endif

    if(present(DelRange) )then
        drange=DelRange
    else
        drange=10
    endif

    if( present(dim) .and. dim/=2 )then
        call MPIData%End()
        stop   "AssembleSurfaceElement :: >> only 2-D is available."
    endif

    n=size(obj%FEMDomain%Mesh%NodCoord,1 )
    allocate(buffer(n,3))
    buffer(:,:)=0.0d0
    allocate(checked(n),r_value(n) )
    checked(:)=0

    buffer(1,:)=obj%FEMDomain%Mesh%NodCoord(1,:)
    checked(1)=1

    do i=1,n-1
        ! get buffer(i+1,:)
        trial_num=0
        do j=1,n
            if(checked(j)==1 )then
                cycle
            endif
            trial_num=trial_num+1
            x(:)   =buffer(i,:)
            x_tr(:)=obj%FEMDomain%Mesh%NodCoord(j,:)
            r_tr=dsqrt(dot_product(x-x_tr,x-x_tr ) )
            if(trial_num==1)then
                r_ref=r_tr
                id_tr=j
            else
                if(r_ref > r_tr)then
                    id_tr=j
                    r_ref=r_tr
                else
                    cycle
                endif
            endif
        enddo
        buffer(i+1,:)=obj%FEMDomain%Mesh%NodCoord(id_tr,:)
        checked(id_tr)=1
    enddo


    ! remove unnatural pixcel
    checked(:)=0
    do i=1,size(buffer,1)-1
        x(:)   =buffer(i  ,:)
        x_tr(:)=buffer(i+1,:)
        r_tr=dsqrt(dot_product(x-x_tr,x-x_tr ) )
        if(r_tr > dble(r_threshold))then
            checked(i)=1
            do k=1,drange
                if(i+k <= size(buffer,1))then
                    checked(i+k)=1
                endif
                if(i-k >= 1)then
                    checked(i-k)=1
                endif

            enddo
            print *, i,i+1
        endif
    enddo

    if(maxval(checked)/=0 )then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
        allocate(obj%FEMDomain%Mesh%NodCoord( n - sum(checked) ,3) )

        k=0
        do i=1,n
            if(checked(i)/=0 )then
                cycle
            else
                k=k+1
                obj%FEMDomain%Mesh%NodCoord(k,1:2)=buffer(i,1:2)
                
            endif
        enddo
    else
        obj%FEMDomain%Mesh%NodCoord(:,:)=buffer(:,:)
    endif
    



    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".txt"
    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".txt"    
    endif

    open(fh,file=python_buffer,status="replace")
    do i=1,size(obj%FEMDomain%Mesh%NodCoord,1)
        write(fh,*) obj%FEMDomain%Mesh%NodCoord(i,1:2)
    enddo
    close(fh)

end subroutine
! #########################################################



! #########################################################
subroutine ReduceSize(obj,MPIData,interval,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in)    :: Name
    integer,intent(in) :: interval
    character*200   :: python_buffer
    character*20    :: pid
    real(8),allocatable:: buffer(:,:)
    integer,allocatable:: selected(:)
    integer :: i,j,k,n,m,fh

    n=size(obj%FEMDomain%Mesh%NodCoord,1)  
    m=size(obj%FEMDomain%Mesh%NodCoord,2)   
    allocate(selected(n) )
    selected(:)=0
    k=0
    do i=1,n
        k=k+1
        if(k==interval)then
            selected(i)=1
            k=0
        else
            cycle
        endif 
    enddo

    allocate(buffer( sum(selected),3 ))

    k=0
    do i=1,n
        if(selected(i)==1 )then
            k=k+1
            buffer(k,:)= obj%FEMDomain%Mesh%NodCoord(i,:)
        endif
    enddo
    
    deallocate(obj%FEMDomain%Mesh%NodCoord)

    allocate(obj%FEMDomain%Mesh%NodCoord( size(buffer,1),size(buffer,2) ) )
    obj%FEMDomain%Mesh%NodCoord(:,:)=buffer(:,:)

    

    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".txt"

    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".txt"    
    endif
    open(fh,file=python_buffer,status="replace")
    do i=1,size(obj%FEMDomain%Mesh%NodCoord,1)
        write(fh,*) obj%FEMDomain%Mesh%NodCoord(i,1:2)
    enddo
    close(fh)

    
end subroutine
! #########################################################



! #########################################################
subroutine ExportGeoFile(obj,MPIData,Name)
    class(PreProcessing_),intent(inout):: obj
    character(*),optional,intent(in)    :: Name
    class(MPI_),intent(inout)          :: MPIData
    character*200   :: python_buffer
    character*20    :: pid
    integer :: i,j,k,n,fh,xsize
    real(8) :: x_p,y_p

    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".geo"

    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".geo"
        
    endif

	xsize = size(obj%FEMDomain%Mesh%NodCoord,1)
	open(fh,file=python_buffer)
	do i=1,xsize
        
        x_p=obj%FEMDomain%Mesh%NodCoord(i,1)
        y_p=obj%FEMDomain%Mesh%NodCoord(i,2)
		write(fh,*)"Point(",i,") = {",x_p,",",y_p,",","0, 1.0};"
	enddo
	
	write(fh,*)"Line(1) = {",xsize,",1};"
	do i=2,xsize
		write(fh,*)"Line(",i,") = {",i-1,",",i,"};"
	enddo
	write(fh,*)"Line Loop(",xsize+1,") = {"
	do i=1,xsize-1
		write(fh,*)i,","
	enddo
	write(fh,*)xsize,"};"
    write(fh,*)"Plane Surface(",xsize+2,") = {",xsize+1,"};"
    flush(fh)
	close(fh)
	!print *, "Done !!"
	!print *, "Generating mesh..."
	!call execute_command_line("gmsh.exe pm2.geo -2 -algo del2d -clmin 40")
end subroutine
! #########################################################



! #########################################################
subroutine ConvertGeo2Msh(obj,MPIData,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in)    :: Name
    character*200   :: python_buffer
    character*200   :: command
    character*20    :: pid
    integer :: i,j,k,n,fh

    write(pid,*) MPIData%MyRank

    fh=MPIData%MyRank+10
    python_buffer=" "
    command  = " "
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".geo"
    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".geo"
    endif
    command="gmsh.exe "//trim(python_buffer)//" -2 -algo del2d -clmin 100"

    writE(*,'(A)') trim(command)
    
    call execute_command_line(command)

    !call execute_command_line("sh ./MakeMesh.sh")
    


end subroutine
! #########################################################


! #########################################################
subroutine ConvertGeo2Inp(obj,MPIData,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in)    :: Name
    character*200   :: python_buffer
    character*200   :: command
    character*20    :: pid
    integer :: i,j,k,n,fh

    write(pid,*) MPIData%MyRank

    fh=MPIData%MyRank+10
    python_buffer=" "
    command  = " "
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".geo"
    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".geo"
    endif
    command="gmsh.exe "//trim(python_buffer)//" -2 -algo del2d -clmin 100 -format inp" 

    writE(*,'(A)') trim(command)
    
    call execute_command_line(trim(command))
    !call execute_command_line("sh ./MakeMesh.sh")
    


end subroutine
! #########################################################

! #########################################################
subroutine ConvertGeo2Mesh(obj,MPIData,SizePara,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    integer,optional,intent(in) :: SizePara
    character(*),optional,intent(in)    :: Name
    character*200   :: python_buffer
    character*200   :: command
    character*20    :: pid,a
    integer :: i,j,k,n,fh,sp

    

    if(present(SizePara) )then
        sp=SizePara
    else
        sp=10
    endif
    write (a,*) sp


    write(pid,*) MPIData%MyRank

    fh=MPIData%MyRank+10
    python_buffer=" "
    command  = " "
    python_buffer="GetSurface_pid_"//trim(adjustl(pid))//".geo"
    if(present(Name) )then
        python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))//".geo"
    endif
    command="gmsh.exe "//trim(python_buffer)//" -2 -algo del2d -clmin"//trim(a)//" -format mesh" 

    writE(*,'(A)') trim(command)
    
    call execute_command_line(command)

    !call execute_command_line("./MakeMesh.sh")
    
    


end subroutine
! #########################################################

! #########################################################
subroutine ConvertMsh2Scf(obj,MPIData,ElementType,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character*200   :: python_buffer
    character*200   :: command,infile,outfile
    character*200,optional,intent(in) :: ElementType
    character(*),optional,intent(in)    :: Name
    character*20    :: pid
    character*11 MeshFormat
	character*14 EndMeshFormat
	character*6  Nodes
	character*9  EndNodes,Elements
    character*12  EndElements	
    integer,allocatable :: elem1(:),surface_nod(:)
    integer :: i,j,k,n,n1,n2,fh,a,nm,mm,nod_num,nn,elem_num,surf_num
    integer :: elem_num_all,n3,n4,n5,n6,n7,elemnod_num,startfrom
    real(8) :: re1,re2
    

    ! ======================================================
    ! deallocate all
    if(allocated(obj%FEMDomain%Mesh%NodCoord ))then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
    endif
    
    if(allocated(  obj%FEMDomain%Mesh%ElemNod ))then
        deallocate(obj%FEMDomain%Mesh%ElemNod)
    endif
    ! ======================================================
    


    ! ======================================================
    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    infile="GetSurface_pid_"//trim(adjustl(pid))//".msh"
	outfile = "GetSurface_pid_"//trim(adjustl(pid))//".scf"
    
    if(present(Name) )then
        infile  = Name//"GetSurface_pid_"//trim(adjustl(pid))//".msh"
	    outfile = Name//"GetSurface_pid_"//trim(adjustl(pid))//".scf"
    endif
    
    open(fh,file=infile,status="old")
    print *, "Opening ",trim(infile)
	! ======================================================
    
    ! ======================================================
	!read file to get nod and elem number
	read(fh,*)MeshFormat
	read(fh,*)re1,nm,mm
	read(fh,*)EndMeshFormat
	read(fh,*)Nodes
    if(nodes/="$Nodes")then
        if(nodes == "$Entit")then
            do 
                read(fh,*)nodes
                if(nodes == "$Nodes")then
                    print *, "Read entities"
                    exit
                endif
            enddo
        else
            stop  "ERROR: invalid location:$Nodes"
        endif
	endif
	! ======================================================
	! Number of nodes
	read(fh,*)nod_num
	!print *,"nod_number",nod_num
    if(allocated(obj%FEMDomain%Mesh%NodCoord))then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
    endif
    allocate(obj%FEMDomain%Mesh%NodCoord(nod_num,3) )
	do i=1,nod_num
        read(fh,*) a
        if(i/=a)then
            stop "ERROR :: msh2scf"
        endif
	enddo
	read(fh,*)EndNodes
	
    if(EndNodes/="$EndNodes")then
		stop  "ERROR: invalid location:$EndNodes"
    endif
    ! ======================================================
    

    ! ======================================================
	read(fh,*)Elements
	if(Elements/="$Elements")then
		stop  "ERROR: invalid location: $Elements"
	endif
	
    read(fh,*)elem_num
    if(present(ElementType) )then
        if(trim(ElementType)=="LinearRectangularGp4")then
            elemnod_num=  4
        elseif(trim(ElementType)=="LinearHexahedralGp8")then
            elemnod_num= 8
        else
            print *, "PreProcessingClass.f90  >> Element : ",ElementType,"is not defined."
            return
        endif
    else
        ! default
        elemnod_num=4
    endif

    startfrom=0
    k=0
    do i=1,elem_num
        read(fh,*)n1,n2,n3
        if(n2==3 .and. n3==2)then
            if(startfrom==0)then
                startfrom=i
            endif
            k=k+1
        endif
    enddo
    allocate(obj%FEMDomain%Mesh%ElemNod(k,elemnod_num) )
    allocate( elem1(elemnod_num) )
    
    read(fh,*)EndElements
	if(EndElements/="$EndElements")then
		stop  "ERROR: invalid location: $EndElements"
    endif	
    close(fh)
    ! ========================================================


    ! ======================================================
    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    infile="GetSurface_pid_"//trim(adjustl(pid))//".msh"
	outfile = "GetSurface_pid_"//trim(adjustl(pid))//".scf"
    if(present(Name) )then
        
        infile  = Name//"GetSurface_pid_"//trim(adjustl(pid))//".msh"
	    outfile = Name//"GetSurface_pid_"//trim(adjustl(pid))//".scf"
    
    endif
    

       
    print *, "File Information is imported."
	open(fh,file=infile,status="old")
	
	! ======================================================
    
    ! ======================================================
	!read file to get nod and elem number
	read(fh,*)MeshFormat
	read(fh,*)re1,nm,mm
	read(fh,*)EndMeshFormat
	read(fh,*)Nodes
	if(nodes/="$Nodes")then
		stop  "ERROR: invalid location:$Nodes"
	endif
	
	! ======================================================
	! Number of nodes
	read(fh,*)nod_num
	!print *,"nod_number",nod_num
    if(allocated(obj%FEMDomain%Mesh%NodCoord))then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
    endif
    allocate(obj%FEMDomain%Mesh%NodCoord(nod_num,3) )
	do i=1,nod_num
		read(fh,*) n2,obj%FEMDomain%Mesh%NodCoord(i,:)
	enddo
	read(fh,*)EndNodes
	
    if(EndNodes/="$EndNodes")then
		stop  "ERROR: invalid location:$EndNodes"
    endif
    ! ======================================================
    

    ! ======================================================
	read(fh,*)Elements
	if(Elements/="$Elements")then
		stop  "ERROR: invalid location: $Elements"
	endif
	
    read(fh,*)elem_num
    if(present(ElementType) )then
        if(trim(ElementType)=="LinearRectangularGp4")then
            elemnod_num=  4  
        elseif(trim(ElementType)=="LinearHexahedralGp8")then
            elemnod_num= 8
        else
            print *, "PreProcessingClass.f90  >> Element : ",ElementType,"is not defined."
            return
        endif
    else
        ! default
        elemnod_num=4
    endif

    k=0
    do i=1,elem_num
        if(startfrom > i )then
            read(fh,*)n1,n2,n3
            cycle
        endif

        if(i <=  size(obj%FEMDomain%Mesh%ElemNod,1)+startfrom-1 )then
            k=k+1
            read(fh,*)n1,n2,n3,n4,n5,obj%FEMDomain%Mesh%ElemNod(k,1:elemnod_num)
            cycle
        endif

        if(size(obj%FEMDomain%Mesh%ElemNod,1)+startfrom >= i)then
            read(fh,*) n1
        endif
    enddo
    
    read(fh,*)EndElements
    if(EndElements/="$EndElements")then
		stop  "ERROR: invalid location: $EndElements"
	endif		
    close(fh)
    
	! ========================================================

	

    ! Setup FEMDomain
    
end subroutine
! #########################################################









! #########################################################
subroutine ConvertMesh2Scf(obj,MPIData,ElementType,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),intent(inout)          :: MPIData
    character(*),optional,intent(in) :: Name
    character*200   :: python_buffer
    character*200   :: command,infile,outfile
    character*200,optional,intent(in) :: ElementType
    character*20    :: pid
    character*11 MeshFormat
	character*14 EndMeshFormat
	character*6  Nodes
	character*9  EndNodes,Elements
    character*12  EndElements	
    integer,allocatable :: elem1(:),surface_nod(:),triangle(:,:),devide_line(:,:)
    integer :: i,j,k,n,n1,n2,fh,a,nm,mm,nod_num,nn,elem_num,surf_num,l
    integer :: elem_num_all,n3,n4,n5,n6,n7,elemnod_num,startfrom,node1,node2,tr1,tr2
    real(8) :: re1,re2
    

    obj%FEMDomain%Mesh%ElemType = "LinearRectangularGp4"
    ! ======================================================
    ! deallocate all
    if(allocated(obj%FEMDomain%Mesh%NodCoord ))then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
    endif
    
    if(allocated(  obj%FEMDomain%Mesh%ElemNod ))then
        deallocate(obj%FEMDomain%Mesh%ElemNod)
    endif
    ! ======================================================
    


    ! ======================================================
    write(pid,*) MPIData%MyRank
    fh=MPIData%MyRank+10
    infile="GetSurface_pid_"//trim(adjustl(pid))//".mesh"
    outfile = "GetSurface_pid_"//trim(adjustl(pid))//".scf"
    if(present(Name) )then
        infile  = Name//"GetSurface_pid_"//trim(adjustl(pid))//".mesh"
        outfile = Name//"GetSurface_pid_"//trim(adjustl(pid))//".scf"
        
    endif
	
    open(fh,file=infile,status="old")
    print *, "Opening ",trim(infile)
	! ======================================================
    
    ! ======================================================
	!read file to get nod and elem number
    read(fh,*)MeshFormat
    read(fh,*)MeshFormat
    read(fh,*)mm
    read(fh,*)MeshFormat

	! ======================================================
	! Number of nodes
	read(fh,*)nod_num
	!print *,"nod_number",nod_num
    if(allocated(obj%FEMDomain%Mesh%NodCoord))then
        deallocate(obj%FEMDomain%Mesh%NodCoord)
    endif
    allocate(obj%FEMDomain%Mesh%NodCoord(nod_num,3) )
    obj%FEMDomain%Mesh%NodCoord(:,:)=0.0d0
	do i=1,nod_num
		read(fh,*) obj%FEMDomain%Mesh%NodCoord(i,:)
    enddo
    
	read(fh,*)EndNodes
    
    ! ======================================================
    
    read(fh,*)mm
    do i=1,mm
        read(fh,*) elem_num
    enddo
    read(fh,*)EndNodes
    ! ======================================================
    
    

    ! ======================================================
    read(fh,*)mm
    allocate(triangle(mm,4),devide_line(mm,3) )
    devide_line(:,:)=-1
    
    do i=1,mm
        read(fh,*) triangle(i,1:3)
        triangle(i,4)=-1
    enddo
    read(fh,*)EndNodes
    ! ======================================================
    ! ======================================================
    read(fh,*)elem_num

    allocate(obj%FEMDomain%Mesh%ElemNod(elem_num,4))
    obj%FEMDomain%Mesh%ElemNod(:,:)=-1
    do i=1,elem_num
        read(fh,*) obj%FEMDomain%Mesh%ElemNod(i,1:4)
    enddo
    ! ======================================================


    ! ======================================================
    
    if(allocated(obj%FEMDomain%Mesh%ElemMat) )then
        deallocate(obj%FEMDomain%Mesh%ElemMat)
    endif
    allocate(obj%FEMDomain%Mesh%ElemMat(elem_num))
    obj%FEMDomain%Mesh%ElemMat(:)=1
    ! ======================================================
    
    ! convert triangle 
    do i=1,size(devide_line,1)
        do j=1,3
            if(i==1)then
                node1=triangle(i,1)
                node2=triangle(i,2)
            elseif(i==2)then
                node1=triangle(i,2)
                node2=triangle(i,3)
            else
                node1=triangle(i,3)
                node2=triangle(i,1)
            endif
            do k=1,size(obj%FEMDomain%Mesh%ElemNod,1)
                do l=1,size(obj%FEMDomain%Mesh%ElemNod,2)
                    if(l==1)then
                        tr1=obj%FEMDomain%Mesh%ElemNod(k, 1)
                        tr2=obj%FEMDomain%Mesh%ElemNod(k, 2)
                    elseif(l==2)then
                        tr1=obj%FEMDomain%Mesh%ElemNod(k, 2)
                        tr2=obj%FEMDomain%Mesh%ElemNod(k, 3)
                    elseif(l==3)then
                        tr1=obj%FEMDomain%Mesh%ElemNod(k, 3)
                        tr2=obj%FEMDomain%Mesh%ElemNod(k, 4)
                    elseif(l==4)then
                        tr1=obj%FEMDomain%Mesh%ElemNod(k, 4)
                        tr2=obj%FEMDomain%Mesh%ElemNod(k, 1)
                    else
                        stop "ERROR :: ConvertMesh2Scf"
                    endif
                    if(node2==tr1 .and. node1 == tr2)then
                        devide_line(i,j)=1
                    endif
                enddo
            enddo
        enddo
    enddo

    !call ShowArray(obj%FEMDomain%Mesh%NodCoord,triangle(:,1:3),20)
    !call ShowArray(obj%FEMDomain%Mesh%NodCoord,obj%FEMDomain%Mesh%ElemNod,30)
    !do i=1,size(devide_line,1)
    !    print *, devide_line(i,:)
    !enddo
    !stop "now debugging"
    


end subroutine
! #########################################################










! #########################################################
subroutine ExportPreProcessing(obj,MPIData,FileName,MeshDimension,Name)
    class(PreProcessing_),intent(inout):: obj
    class(MPI_),optional,intent(inout) :: MPIData
    character*200,optional,intent(in)  :: FileName
    character(*),optional,intent(in)   :: Name
    integer,optional,intent(in) :: MeshDimension
    character*200   :: python_buffer
    character*200   :: command
    character*20    :: pid
    integer :: i,j,k,n,fh

    fh=11
    if(present(MPIData) )then
        write(pid,*) MPIData%MyRank
        fh=MPIData%MyRank+120
        if(present(Name) )then
            python_buffer=Name//"GetSurface_pid_"//trim(adjustl(pid))
        else
            python_buffer="GetSurface_pid_"//trim(adjustl(pid))
        
        endif
    elseif(present(FileName) )then
        if(present(Name) )then
            python_buffer=Name//trim(FileName)
        else
            python_buffer=trim(FileName)
        endif
    else
        if(present(Name) )then
            python_buffer=Name
        else
            python_buffer="NoName"
        endif
    endif

    call obj%FEMDomain%Export(OptionalProjectName=python_buffer,FileHandle=fh)
    
end subroutine
! #########################################################


!##################################################
subroutine SetSolverPreProcessing(obj,inSolverType)
    class(PreProcessing_),intent(inout)::obj
    character*200,optional,intent(in) :: inSolverType
    character*200 ::sn

    if( present(inSolverType) )then
        sn = inSolverType
    else
        sn = "Default"
    endif



    Obj%FEMDomain%SolverType=sn
    
end subroutine
!##################################################



!##################################################
subroutine SetDataTypeFEMDomain(obj,inDType)
    class(PreProcessing_),intent(inout)::obj
    character*200,optional,intent(in) :: inDType
    character*200 :: sn

    sn=""

    if( .not.present(inDType) )then
        sn = "FEMDomain"
    else
        sn = inDType
    endif
    
    call obj%FEMDomain%SetDataType(trim(sn) )

end subroutine

!##################################################


!##################################################
subroutine SetUpPreprocessing(obj,DataType,SolverType,NoFacetMode,MatPara)
    class(PreProcessing_),intent(inout)::obj
    character*200,optional,intent(in) :: DataType
    character*200,optional,intent(in) :: SolverType
    logical,optional,intent(in) :: NoFacetMode
    real(8),allocatable,optional,intent(inout)::MatPara(:,:)
    real(8),allocatable::MatParaDef(:,:)
    character*200 :: sn


    if(present(DataType) )then
        call obj%SetDataType(DataType)
    else
        call obj%SetDataType()
    endif

    call obj%FEMDomain%Mesh%Init(NoFacetMode=NoFacetMode)
    if(.not.present(MatPara) )then
        allocate(MatParaDef(1,1) )
        MatParaDef(:,:)=100.0d0
        call obj%FEMDomain%MaterialProp%Init(MaterialParameters=MatParaDef)
    else
        call obj%FEMDomain%MaterialProp%Init(MaterialParameters=MatPara)
    endif


end subroutine
!##################################################


!##################################################
subroutine SetScalePreProcessing(obj,scalex,scaley,scalez&
    ,picscalex,picscaley,picscalez)
    class(PreProcessing_),intent(inout)::obj
    real(8),optional,intent(in)::scalex,scaley,scalez
    real(8),optional,intent(in)::picscalex,picscaley,picscalez
    real(8) :: lx,ly,lz
    integer :: i

    if(present(scalex) )then
        if(scalex == 0.0d0)then
            stop "ERROR :: SetScalePreProcessing >> scalex==0.0d0"
            return
        else
            lx=maxval(obj%FEMDomain%Mesh%NodCoord(:,1) ) -&
                minval(obj%FEMDomain%Mesh%NodCoord(:,1) )
            if(lx==0.0d0)then
                stop "ERROR :: SetScalePreProcessing >> lx==0.0d0"
            endif    
            obj%FEMDomain%Mesh%NodCoord(:,1)=&
            obj%FEMDomain%Mesh%NodCoord(:,1)/lx*scalex
        endif
    endif

    if(present(scaley) )then
        if(scaley == 0.0d0)then
            stop "ERROR :: SetScalePreProcessing >> scaley==0.0d0"
            return
        else
            ly=maxval(obj%FEMDomain%Mesh%NodCoord(:,2) ) -&
                minval(obj%FEMDomain%Mesh%NodCoord(:,2) )
            if(ly==0.0d0)then
                stop "ERROR :: SetScalePreProcessing >> ly==0.0d0"
            endif    
            obj%FEMDomain%Mesh%NodCoord(:,2)=&
            obj%FEMDomain%Mesh%NodCoord(:,2)/ly*scaley
        endif
    endif
    if(present(scalez) )then
        if(scalez == 0.0d0)then
            stop "ERROR :: SetScalePreProcessing >> scalez==0.0d0"
            return
        else
            lz=maxval(obj%FEMDomain%Mesh%NodCoord(:,3) ) -&
                minval(obj%FEMDomain%Mesh%NodCoord(:,3) )
            if(lz==0.0d0)then
                stop "ERROR :: SetScalePreProcessing >> lz==0.0d0"
            endif    
            obj%FEMDomain%Mesh%NodCoord(:,3)=&
            obj%FEMDomain%Mesh%NodCoord(:,3)/lz*scalez
        endif
    endif

    do i=1,size(obj%FEMDomain%Mesh%NodCoord,1)
        write(123,*) obj%FEMDomain%Mesh%NodCoord(i,:)
    enddo
    
end subroutine
!##################################################



!##################################################
subroutine SetBoundaryConditionPrePro(obj,Dirichlet,Neumann,Initial,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,val,val_id,NumOfValPerNod)
    class(PreProcessing_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    logical,optional,intent(in)::Dirichlet,Neumann,Initial
    integer,optional,intent(in)::NumOfValPerNod,val_id
    real(8),optional,intent(in)::val
    integer :: i,j,n
    if(present(Dirichlet) )then
        if(Dirichlet .eqv. .true.)then
            
            call AddDBoundCondition(obj%FEMDomain,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax&
            ,zmin=zmin,zmax=zmax,tmin=tmin,tmax=tmax,val=val,val_id=val_id,NumOfValPerNod=NumOfValPerNod)
            return

        endif
    endif    
    
    if(present(Neumann) )then
        if(Neumann .eqv. .true.)then
            
            call AddNBoundCondition(obj%FEMDomain,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax&
            ,zmin=zmin,zmax=zmax,tmin=tmin,tmax=tmax,val=val,val_id=val_id,NumOfValPerNod=NumOfValPerNod)
            
            return
        endif
    endif
    
    if(present(Initial) )then
        if(Initial .eqv. .true.)then
            
            call AddTBoundCondition(obj%FEMDomain,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax&
            ,zmin=zmin,zmax=zmax,tmin=tmin,tmax=tmax,val=val,val_id=val_id,NumOfValPerNod=NumOfValPerNod)
            
            return
        endif
    endif

end subroutine
!##################################################

!##################################################
subroutine SetSizeOfBCPrePrecessing(obj,Dirichlet,Neumann,Initial,NumOfValue)
    class(PreProcessing_),intent(inout)::obj
    
    logical,optional,intent(in)::Dirichlet,Neumann,Initial
    integer,optional,intent(in)::NumOfValue
    logical :: DB,NB,IC
    integer :: coord_dim

    if(.not. present(Dirichlet) )then
        DB = .false.
    else
        DB=Dirichlet
    endif

    if(.not. present(Neumann) )then
        NB = .false.
    else
        NB=Neumann
    endif


    if(.not. present(Initial) )then
        IC = .false.
    else
        IC=Initial
    endif

    coord_dim = size(obj%FEMDomain%Mesh%NodCoord,2)
    if(DB .eqv. .true.)then
        call obj%FEMDomain%InitDBC(NumOfValue)
    elseif(NB .eqv. .true.)then
        call obj%FEMDomain%InitNBC(NumOfValue)
    elseif(IC .eqv. .true.)then
        call obj%FEMDomain%InitTBC(NumOfValue)
    else
        return
    endif


end subroutine
!##################################################

!##################################################
subroutine ShowBCPrePrecessing(obj,Dirichlet,Neumann,Initial)
    class(PreProcessing_),intent(inout)::obj
    
    logical,optional,intent(in)::Dirichlet,Neumann,Initial

    integer :: n,m1,m2,i

    if(Dirichlet .eqv. .true.)then
        n=size(obj%FEMDomain%Boundary%DBoundNum)
        m1=size(obj%FEMDomain%Boundary%DBoundNodID,1)
        m2=size(obj%FEMDomain%Boundary%DBoundVal,1)
        print *, "Number of boundary conditions are :: ", n
        print *, "obj%FEMDomain%Boundary%DBoundNodID"
        do i=1,m1
            print *, obj%FEMDomain%Boundary%DBoundNodID(i,:)
        enddo
        do i=1,m2
            print *, obj%FEMDomain%Boundary%DBoundVal(i,:)
        enddo
        
    endif

end subroutine


!##################################################



!##################################################
subroutine Convert3Dto2D(obj)
    class(PreProcessing_),intent(inout)::obj
    real(8),allocatable::buffer(:,:)
    integer :: i,n,m

    n=size(obj%FEMDomain%Mesh%NodCoord,1)
    m=size(obj%FEMDomain%Mesh%NodCoord,2)

    allocate(buffer(n,2))

    do i=1,n
        buffer(i,1:2)=obj%FEMDomain%Mesh%NodCoord(i,1:2)
    enddo
    deallocate(obj%FEMDomain%Mesh%NodCoord)
    allocate(obj%FEMDomain%Mesh%NodCoord(n,2) )
    obj%FEMDomain%Mesh%NodCoord(:,:)=buffer(:,:)
    deallocate(buffer)
end subroutine
!##################################################


!##################################################
subroutine Convert2Dto3D(obj,Thickness,division)
    class(PreProcessing_),intent(inout)::obj
    real(8),allocatable::buffer(:,:)
    real(8),optional,intent(in)::Thickness
    integer,optional,intent(in)::division
    real(8) :: Tn
    integer :: i,j,n,m,NumOfLayer,numnod


    ! only for linear elements

    if(present(Thickness))then
        if(Thickness==0.0d0)then
            print *, "ERROR :: Convert2Dto3D >> Thickness = 0"
            return
        else
            Tn=Thickness
        endif
    else
        Tn=1.0d0
    endif

    if(present(division))then
        if(division==0)then
            print *, "ERROR :: Convert2Dto3D >> division = 0"
            return
        endif
        NumOfLayer=division
    else
        NumOfLayer=1
    endif

    numnod=size(obj%FEMDomain%Mesh%NodCoord,1)
    n=size(obj%FEMDomain%Mesh%NodCoord,1)
    m=size(obj%FEMDomain%Mesh%NodCoord,2)

    allocate(buffer(n*(NumOfLayer+1),3))

    do j=1,NumOfLayer+1
        do i=1,n
            buffer( n*(j-1) + i ,1:2) = obj%FEMDomain%Mesh%NodCoord(i,1:2)
            buffer( n*(j-1) + i ,3)   = Tn / dble(NumOfLayer)*dble(j-1)
        enddo
    enddo

    deallocate(obj%FEMDomain%Mesh%NodCoord)
    allocate(obj%FEMDomain%Mesh%NodCoord( size(buffer,1) ,size(buffer,2) ) )
    obj%FEMDomain%Mesh%NodCoord(:,:)=buffer(:,:)
    deallocate(buffer)


    ! ElemNod

    if(.not.allocated(obj%FEMDomain%Mesh%ElemNod) )then
        print *, "Caution :: Convert2Dto3D >> ElemNod is not allocated = 0"
        return
    endif
    n=size(obj%FEMDomain%Mesh%ElemNod,1)
    m=size(obj%FEMDomain%Mesh%ElemNod,2)

    allocate(buffer(n*NumOfLayer,m*2))

    do j=1,NumOfLayer
        do i=1,n
            buffer( n*(j-1)+i, 1:m      ) = obj%FEMDomain%Mesh%ElemNod(i,1:m)+numnod*(j-1)
            buffer( n*(j-1)+i, m+1:2*m  ) = obj%FEMDomain%Mesh%ElemNod(i,1:m)+numnod*(j)
        enddo
    enddo

    deallocate(obj%FEMDomain%Mesh%ElemNod)
    allocate(obj%FEMDomain%Mesh%ElemNod( size(buffer,1) ,size(buffer,2) ) )
    obj%FEMDomain%Mesh%ElemNod(:,:)=buffer(:,:)
    deallocate(buffer)

    ! ElemMat

    if(.not.allocated(obj%FEMDomain%Mesh%ElemMat) )then
        print *, "Caution :: Convert2Dto3D >> ElemMat is not allocated = 0"
        return
    endif

    allocate(buffer(n*NumOfLayer,1))

    do j=1,NumOfLayer
        do i=1,n
            buffer( n*(j-1)+i, 1      ) = obj%FEMDomain%Mesh%ElemMat(i)
        enddo
    enddo

    deallocate(obj%FEMDomain%Mesh%ElemMat)
    allocate(obj%FEMDomain%Mesh%ElemMat( size(buffer,1) ) )
    obj%FEMDomain%Mesh%ElemMat(:)=buffer(:,1)
    deallocate(buffer)
    
    

end subroutine
!##################################################

!##################################################
subroutine SetControlParaPrePro(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(PreProcessing_),intent(inout)::obj
    real(8),optional,intent(in)::OptionalTol
    integer,optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep
    
    call SetControlPara(obj%FEMDomain%ControlPara,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
end subroutine
!##################################################

!##################################################
subroutine SetMatParaPreProcessing(obj,NumOfMaterial,NumOfPara,MaterialID,ParameterID,Val,AllVal)
    class(PreProcessing_),intent(inout)::obj
    integer,optional,intent(in)::NumOfMaterial,NumOfPara,MaterialID,ParameterID
    real(8),optional,intent(in)::Val,AllVal(:)
    integer ::i,n,m,p(2),mm


    if(.not.allocated(obj%FEMDomain%MaterialProp%MatPara) )then
        allocate(obj%FEMDomain%MaterialProp%MatPara(1,1) )
        obj%FEMDomain%MaterialProp%MatPara(1,1)=0.0d0
    endif
    
    if(present(NumOfMaterial) )then
        if(NumOfMaterial > size(obj%FEMDomain%MaterialProp%MatPara,1) )then
            mm=size(obj%FEMDomain%MaterialProp%MatPara,1)
            do i=1,NumOfMaterial - mm
                call extendArray(obj%FEMDomain%MaterialProp%MatPara,extend1stColumn=.true.)

                
                if(present(val) )then
                    obj%FEMDomain%MaterialProp%MatPara(mm+i,:)=val
                endif
                
                n=size(obj%FEMDomain%MaterialProp%MatPara,2)

                if(present(AllVal) )then
                    m=size(AllVal)
                    p(1)=n
                    p(2)=m
                    obj%FEMDomain%MaterialProp%MatPara(mm+i,1: minval(p) )=AllVal(1: minval(p) )
                endif
                
            enddo
        endif
    endif

    
    if(present(NumOfPara) )then
        if(NumOfPara > size(obj%FEMDomain%MaterialProp%MatPara,2) )then
            mm=size(obj%FEMDomain%MaterialProp%MatPara,2)
            do i=1,NumOfPara - mm
                
                call extendArray(obj%FEMDomain%MaterialProp%MatPara,extend2ndColumn=.true.)
                if(present(val) )then
                    obj%FEMDomain%MaterialProp%MatPara(:,mm+i)=val
                endif
                n=size(obj%FEMDomain%MaterialProp%MatPara,1)
                if(present(AllVal) )then
                    m=size(AllVal)
                    p(1)=n
                    p(2)=m
                    obj%FEMDomain%MaterialProp%MatPara(1: minval(p) , mm+i)=AllVal(1: minval(p) )
                endif
            enddo
        endif
    endif


end subroutine
!##################################################


!##################################################
subroutine SetMatIDPreProcessing(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,MaterialID)
    class(PreProcessing_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    integer,optional,intent(in)::MaterialID
    integer :: i,j,n

    ! Now implementing
    call AddMaterialID(obj%FEMDomain,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax&
            ,zmin=zmin,zmax=zmax,tmin=tmin,tmax=tmax,MaterialID=MaterialID)
            

end subroutine
!##################################################



!##################################################
subroutine getSkeltonPreProcessing(obj)
    class(PreProcessing_),intent(inout)::obj

    call obj%FEMDomain%MeltingSkelton()

end subroutine
!##################################################


!##################################################
subroutine setEntityPreProcessing(obj,Circle,Rectangle,Plane,Cylinder,Box,&
    Radius,XSize,YSize,ZSize,Xloc,Yloc,Zloc)
    class(PreProcessing_),intent(inout)::obj
    logical,optional,intent(in) :: Circle,Rectangle,Plane,Cylinder,Box
    real(8),optional,intent(in) :: Radius,XSize,YSize,ZSize,Xloc,Yloc,Zloc
    integer :: i


    if( present(Circle) )then
        if(Circle .eqv. .true.)then
            print *, "Under construction"    
            return
        endif
    endif

    if( present(Rectangle) )then
        if(Rectangle .eqv. .true.)then
            if(.not. present(Xsize) )then
                print *, "Error :: setEntity >> Please import XSize"
                return
            endif
            if(.not. present(Ysize) )then
                print *, "Error :: setEntity >> Please import YSize"
                return
            endif

            if(allocated(obj%FEMDomain%Mesh%NodCoord) )then
                deallocate(obj%FEMDomain%Mesh%NodCoord)
            endif
            if(allocated(obj%FEMDomain%Mesh%ElemNod) )then
                deallocate(obj%FEMDomain%Mesh%ElemNod)
            endif
            if(allocated(obj%FEMDomain%Mesh%ElemMat) )then
                deallocate(obj%FEMDomain%Mesh%ElemMat)
            endif
            allocate(obj%FEMDomain%Mesh%NodCoord(4,2) )
            obj%FEMDomain%Mesh%NodCoord(1,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(1,2)=0.0d0
            obj%FEMDomain%Mesh%NodCoord(2,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(2,2)=0.0d0
            obj%FEMDomain%Mesh%NodCoord(3,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(3,2)=ysize
            obj%FEMDomain%Mesh%NodCoord(4,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(4,2)=ysize

            allocate(obj%FEMDomain%Mesh%ElemNod(1,4) )
            do i=1,4
                obj%FEMDomain%Mesh%ElemNod(1,i)=i
            enddo
            allocate(obj%FEMDomain%Mesh%ElemMat(1) )
            obj%FEMDomain%Mesh%ElemMat(1)=1


            if( present(Xloc) )then
                obj%FEMDomain%Mesh%NodCoord(:,1)=obj%FEMDomain%Mesh%NodCoord(:,1)+Xloc                
            endif
            if( present(Yloc) )then
                obj%FEMDomain%Mesh%NodCoord(:,2)=obj%FEMDomain%Mesh%NodCoord(:,2)+Yloc
            endif
            return

        endif
    endif
    if( present(Plane) )then
        if(Plane .eqv. .true.)then
            print *, "Under construction"
            return
        endif
    endif
    if( present(Cylinder) )then
        if(Cylinder .eqv. .true.)then
            print *, "Under construction"
            return
        endif
    endif
    if( present(Box) )then
        if(Box .eqv. .true.)then
            if(.not. present(Xsize) )then
                print *, "Error :: setEntity >> Please import XSize"
                return
            endif
            if(.not. present(Ysize) )then
                print *, "Error :: setEntity >> Please import YSize"
                return
            endif
            if(.not. present(Zsize) )then
                print *, "Error :: setEntity >> Please import YSize"
                return
            endif

            if(allocated(obj%FEMDomain%Mesh%NodCoord) )then
                deallocate(obj%FEMDomain%Mesh%NodCoord)
            endif
            if(allocated(obj%FEMDomain%Mesh%ElemNod) )then
                deallocate(obj%FEMDomain%Mesh%ElemNod)
            endif
            if(allocated(obj%FEMDomain%Mesh%ElemMat) )then
                deallocate(obj%FEMDomain%Mesh%ElemMat)
            endif

            allocate(obj%FEMDomain%Mesh%NodCoord(8,3) )
            obj%FEMDomain%Mesh%NodCoord(1,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(1,2)=0.0d0; obj%FEMDomain%Mesh%NodCoord(1,3)=0.0d0;
            obj%FEMDomain%Mesh%NodCoord(2,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(2,2)=0.0d0; obj%FEMDomain%Mesh%NodCoord(2,3)=0.0d0;
            obj%FEMDomain%Mesh%NodCoord(3,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(3,2)=ysize; obj%FEMDomain%Mesh%NodCoord(3,3)=0.0d0;
            obj%FEMDomain%Mesh%NodCoord(4,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(4,2)=ysize; obj%FEMDomain%Mesh%NodCoord(4,3)=0.0d0;
            obj%FEMDomain%Mesh%NodCoord(5,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(5,2)=0.0d0; obj%FEMDomain%Mesh%NodCoord(5,3)=ZSize;
            obj%FEMDomain%Mesh%NodCoord(6,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(6,2)=0.0d0; obj%FEMDomain%Mesh%NodCoord(6,3)=ZSize;
            obj%FEMDomain%Mesh%NodCoord(7,1)=Xsize; obj%FEMDomain%Mesh%NodCoord(7,2)=ysize; obj%FEMDomain%Mesh%NodCoord(7,3)=ZSize;
            obj%FEMDomain%Mesh%NodCoord(8,1)=0.0d0; obj%FEMDomain%Mesh%NodCoord(8,2)=ysize; obj%FEMDomain%Mesh%NodCoord(8,3)=ZSize;

            allocate(obj%FEMDomain%Mesh%ElemNod(1,8) )

            obj%FEMDomain%Mesh%ElemNod(1,1)=1
            obj%FEMDomain%Mesh%ElemNod(1,2)=2
            obj%FEMDomain%Mesh%ElemNod(1,3)=3
            obj%FEMDomain%Mesh%ElemNod(1,4)=4
            obj%FEMDomain%Mesh%ElemNod(1,5)=5
            obj%FEMDomain%Mesh%ElemNod(1,6)=6
            obj%FEMDomain%Mesh%ElemNod(1,7)=7
            obj%FEMDomain%Mesh%ElemNod(1,8)=8


            allocate(obj%FEMDomain%Mesh%ElemMat(1) )
            obj%FEMDomain%Mesh%ElemMat(1)=1

            if( present(Xloc) )then
                obj%FEMDomain%Mesh%NodCoord(:,1)=obj%FEMDomain%Mesh%NodCoord(:,1)+Xloc                
            endif
            if( present(Yloc) )then
                obj%FEMDomain%Mesh%NodCoord(:,2)=obj%FEMDomain%Mesh%NodCoord(:,2)+Yloc
            endif
            if( present(Zloc) )then
                obj%FEMDomain%Mesh%NodCoord(:,3)=obj%FEMDomain%Mesh%NodCoord(:,3)+Zloc
            endif


            return
        endif
    endif



end subroutine
!##################################################


!##################################################
subroutine BooleanModifyerPreProcessing(obj,ModObj,XDiv,Ydic,Zdiv)
    class(PreProcessing_),intent(inout)::obj
    class(PreProcessing_),intent(inout)::ModObj
    integer,optional,intent(in) :: XDiv,Ydic,Zdiv
    real(8) :: ground_level
    real(8),allocatable::RSInterface(:,:),xmin,xmax,ymin,ymax,NodCoord(:,:)
    integer :: ground_surface_id,n,m,itr,k,i,j,buf(2)
    integer :: NodeNum,DimNum,ElemNum,ElemNodNum,startnode,endnode,newnodenum
    integer,allocatable::RSIElemID(:),RSINodeID(:),RSIElemNod(:,:),AvailFE(:)
    integer,allocatable::OldNodID(:),OldtoNewNodID(:),countnode(:,:)

    ! ###### This is for 4-node elements or 8-node box element
    if(.not.allocated(obj%FEMDomain%Mesh%NodCoord) )then
        print *, "Boolean :: >> Error Please import obj%FEMDomain%Mesh%NodCoord"
        return  
    endif
    if(.not.allocated(Modobj%FEMDomain%Mesh%NodCoord) )then
        print *, "Boolean :: >> Error Please import Modobj%FEMDomain%Mesh%NodCoord"
        return  
    endif

    if(size(obj%FEMDomain%Mesh%NodCoord,2)==2)then
        ! if rectangle => ok
        if(size(obj%FEMDomain%Mesh%ElemNod,2) == 4)then
            ! ### only structural is supported ###
            print *,  "Boolean :: ### only structural is supported ###"
            xmin=minval(obj%FEMDomain%Mesh%NodCoord(:,1) )
            xmax=maxval(obj%FEMDomain%Mesh%NodCoord(:,1) )
            ymin=minval(obj%FEMDomain%Mesh%NodCoord(:,2) )
            ymax=maxval(obj%FEMDomain%Mesh%NodCoord(:,2) )
            ground_level=maxval(obj%FEMDomain%Mesh%NodCoord(:,2) )


            NodeNum=size(obj%FEMDomain%Mesh%NodCoord,1)
            DimNum=size(obj%FEMDomain%Mesh%NodCoord,2)
            ElemNum=size(obj%FEMDomain%Mesh%ElemNod,1)
            ElemNodNum=size(obj%FEMDomain%Mesh%ElemNod,2)
            allocate(RSIElemID(ElemNum),RSINodeID(NodeNum) )
            allocate(OldNodID(NodeNum),OldtoNewNodID(NodeNum))
            OldNodID(:)=0
            OldtoNewNodID(:)=0
            RSIElemID(:)=1
            RSINodeID(:)=-1
            if( .not.allocated(ModObj%FEMDomain%Mesh%FacetElemNod) )then
                call GetSurface(ModObj%FEMDomain%Mesh)
            endif

            call Obj%FEMDomain%Mesh%Copy(ModObj%FEMDomain%Mesh,Minimum=.true.)
            ! call showArray(-Obj%FEMDomain%Mesh%NodCoord,Obj%FEMDomain%Mesh%FacetElemNod,FileHandle=224)



            n=size(ModObj%FEMDomain%Mesh%FacetElemNod,1)
            m=size(ModObj%FEMDomain%Mesh%FacetElemNod,2)
            allocate(AvailFE(n) )
            allocate(countnode( size(ModObj%FEMDomain%Mesh%NodCoord,1) ,2) )
            countnode(:,:)=0
            AvailFE(:)=0
            itr=0
            do i=1,n
                do j=1,m
                    if(Modobj%FEMDomain%Mesh%NodCoord(ModObj%FEMDomain%Mesh%FacetElemNod(i,j) ,2)<=ground_level )then
                        ! utilize
                        countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i,j),1)=countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i,j),1)+1
                        countnode(ModObj%FEMDomain%Mesh%FacetElemNod(i,j),2)=i
                        AvailFE(i)=1
                        itr=itr+1
                        exit
                    endif
                enddo
            enddo

            k=0
            do i=1,size(AvailFE)
                if(k==0 .and. AvailFE(i)==1 )then
                    cycle
                endif

                if(k==0 .and. AvailFE(i)==0)then
                    startnode=i
                    k=1
                endif
                if(k==1 .and. AvailFE(i)==0)then
                    cycle
                endif
                if(k==1 .and. AvailFE(i)==1)then
                    endnode=i
                    exit
                endif

            enddo


            buf(1)=startnode
            buf(2)=endnode
            n=size(Obj%FEMDomain%Mesh%FacetElemNod,1)
            m=size(Obj%FEMDomain%Mesh%NodCoord,2)
            allocate(NodCoord(itr+4,m) )

            NodCoord(:,:)=0.0d0
            itr=0
            do i=1,n

                if( i > minval(buf) )then
                    exit
                endif
                if(AvailFE(i)==1 )then
                    itr=itr+1
                    NodCoord(itr,:)=Obj%FEMDomain%Mesh%NodCoord(Obj%FEMDomain%Mesh%FacetElemNod(i,1) ,: )
                endif


            enddo

            NodCoord(1+itr,1)=xmax        ;NodCoord(1+itr,2)=ymax       ;
            NodCoord(2+itr,1)=xmax        ;NodCoord(2+itr,2)=ymin       ;
            NodCoord(3+itr,1)=xmin        ;NodCoord(3+itr,2)=ymin       ;
            NodCoord(4+itr,1)=xmin        ;NodCoord(4+itr,2)=ymax       ;
            itr=itr+4
            
            

            
            do i=1,n
                
                if( i < maxval(buf) )then
                    cycle
                endif

                if(AvailFE(i)==1 )then
                    itr=itr+1
                    NodCoord(itr,:)=Obj%FEMDomain%Mesh%NodCoord(Obj%FEMDomain%Mesh%FacetElemNod(i,1) ,: )
                endif

                if(itr > n)then
                    exit
                endif
            enddo

            deallocate(Obj%FEMDomain%Mesh%NodCoord)
            allocate(Obj%FEMDomain%Mesh%NodCoord(size(NodCoord,1),size(NodCoord,2) ) )
            do i=1,size(NodCoord,1)
                Obj%FEMDomain%Mesh%NodCoord(i,:)=NodCoord(i,:)
            enddo
            



            !call showArray(NodCoord,FileHandle=226)
            !print *," "
            !print *, startnode,endnode
            return



            if(allocated(obj%FEMDomain%Mesh%ElemNod) )then
                deallocate(obj%FEMDomain%Mesh%ElemNod)
            endif

            allocate(obj%FEMDomain%Mesh%FacetElemNod(itr+4,m) )

            ! FacetElements of under-gournd part of root domain is copyed to soil domain
            itr=0
            do i=1,n
                if(AvailFE(i)==1 )then
                    itr=itr+1
                    obj%FEMDomain%Mesh%FacetElemNod(itr,:)=Modobj%FEMDomain%Mesh%FacetElemNod(i,:)
                    do j=1,m
                        OldNodID( Modobj%FEMDomain%Mesh%FacetElemNod(i,j)  )=1
                    enddo
                endif
            enddo


            ! list up old to new

            itr=0
            do i=1,NodeNum
                if(OldNodID(i)==1 )then
                    itr=itr+1
                    OldtoNewNodID(i)=itr
                endif
            enddo

            do i=1,size(obj%FEMDomain%Mesh%FacetElemNod,1)
                do j=1,size(obj%FEMDomain%Mesh%FacetElemNod,2)
                    obj%FEMDomain%Mesh%FacetElemNod(i,j)=OldtoNewNodID( obj%FEMDomain%Mesh%FacetElemNod(i,j)  )
                    if(obj%FEMDomain%Mesh%FacetElemNod(i,j) == 0 )then
                        stop "BooleanModifyerPreProcessing :: ERROR :: OldtoNewNodID is wrong "
                    endif
                enddo
            enddo



            NewNodeNum=maxval(OldtoNewNodID)
            if(allocated(obj%FEMDomain%Mesh%NodCoord) )then
                deallocate(obj%FEMDomain%Mesh%NodCoord)
            endif
            allocate( obj%FEMDomain%Mesh%NodCoord(NewNodeNum,DimNum) )
            itr=0
            do i=1,NodeNum
                if( OldNodID(i)==1 )then
                    itr=itr+1
                    obj%FEMDomain%Mesh%NodCoord(itr,:)=Modobj%FEMDomain%Mesh%NodCoord(i,:)
                endif
            enddo
            allocate(countnode(NewNodeNum,2) )
            countnode(:,:)=0


            ! NodCoord, FacetElemNod are imported.
            ! Find edge of SurfaceNod
            ! Sort 
            itr=0
            do i=1,size(obj%FEMDomain%Mesh%FacetElemNod,1)
                do j=1,size(obj%FEMDomain%Mesh%FacetElemNod,2)
                    countnode(obj%FEMDomain%Mesh%FacetElemNod(i,j),1 )=&
                    countnode(obj%FEMDomain%Mesh%FacetElemNod(i,j),1 )+1
                    k=countnode(obj%FEMDomain%Mesh%FacetElemNod(i,j),2 )
                    if(k<j)then
                        countnode(obj%FEMDomain%Mesh%FacetElemNod(i,j),2 )=j
                    endif
                    
                enddo
            enddo


            if(maxval(countnode(:,1) )==3 )then
                print *, "Boolean >> ERROR :: Same node appears 3 times "
                stop 
            elseif(minval(countnode(:,1) )==2)then
                print *, "Boolean >> ERROR :: minval(countnode)==2 "
                stop 
            else
                print *,"Boolean >> ERROR :: minval(countnode)<=0"
                stop 
            endif

            itr=0
            do i=1,size(countnode,1)
                if(countnode(i,1) == 1)then
                    itr=itr+1
                    if(countnode(i,2)==1 )then
                        startnode=i
                    elseif(countnode(i,2)==2)then
                        endnode=i
                    else
                        stop "Boolean >> ERROR :: countnode(i,2) /= (1 or 2) "
                    endif
                endif
            enddo

            if(itr /= 2)then
                print *,"Boolean >> ERROR :: itr /= 2"
                stop 
            endif

            ! あとはSurfaceNodを出力するだけ
            open(1233,file="test.txt")
            do i=1, size(obj%FEMDomain%Mesh%FacetElemNod,1)
                n=obj%FEMDomain%Mesh%FacetElemNod(i,1)
                write(1233,*) obj%FEMDomain%Mesh%NodCoord(i,:)
                do j=1,size(obj%FEMDomain%Mesh%FacetElemNod,1)
                    
                enddo
            enddo



        
            
            





            !do i=1,NodeNum
            !    if(obj%FEMDomain%Mesh%NodCoord(i,2) <= ground_level )then
            !        RSINodeID(i)=1
            !    endif
            !enddo
!
            !itr=0
            !do i=1,ElemNum
            !    do j=1,ElemNodNum
            !        if(obj%FEMDomain%Mesh%ElemNod(i,j) == -1 )then
            !            RSIElemID(:)=-1
            !            itr=itr+1
            !            exit
            !        endif
            !    enddo
            !enddo
            !allocate(RSIElemNod(ElemNum-itr,ElemNodNum) )
            !do i=1,ElemNum
            !    if(RSIElemNod(i) == 1 )then
            !        RSIElemNod(i,:)=obj%FEMDomain%Mesh%ElemNod(i,:) 
            !    endif
            !enddo

            

            return
        else
            print *, "Boolean :: >> Error :: only rectangle is supported"
            return  
        endif
    

    elseif(size(obj%FEMDomain%Mesh%NodCoord,2)==3)then
        ! if box => ok
        if(size(obj%FEMDomain%Mesh%ElemNod) == 8)then
            call Modobj%FEMDomain%Mesh%GetSurface()

            stop "Now implementing box"
        else
            print *, "Boolean :: >> Error :: only box is supported"
            return  
        endif
    else
        print *, "Boolean :: >> Error size(obj%FEMDomain%Mesh%NodCoord,2) should be 2 or 3"
        return
    endif

    

end subroutine
!##################################################

subroutine ReversePreProcessing(obj)
    class(PreProcessing_),intent(inout)::obj

    obj%FEMDomain%Mesh%NodCoord(:,:) = -obj%FEMDomain%Mesh%NodCoord(:,:) 
end subroutine
!##################################################




!##################################################
subroutine showMeshPreProcessing(obj,Step,Name)
    class(PreProcessing_),intent(inout)::obj
    character(*),optional,intent(in):: Name
    integer,optional,intent(in):: Step

    integer :: stp


    if(present(Step) )then
        stp=step
    else
        stp=0
    endif

    call GmshPlotMesh(obj%FEMDomain,OptionalStep=stp,Name=Name)

end subroutine
!##################################################


end module


