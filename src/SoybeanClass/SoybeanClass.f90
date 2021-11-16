module SoybeanClass
    use, intrinsic :: iso_fortran_env
    use sim
    use SeedClass
    use LeafClass
    use RootClass
    use SoilClass
    use LightClass
    use PlantNodeClass
    implicit none

    type :: soybean_NodeID_Branch_
        integer(int32),allocatable :: ID(:)
    end type
    
    integer(int32),parameter :: PF_DEFAULT_SOYBEAN_ASIZE=300

    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer(int32) :: Num_Of_Node
        integer(int32) :: num_leaf
        integer(int32) :: num_stem_node
        integer(int32) :: Num_Of_Root
        
        integer(int32) :: MaxLeafNum= PF_DEFAULT_SOYBEAN_ASIZE
        integer(int32) :: MaxRootNum= PF_DEFAULT_SOYBEAN_ASIZE
        integer(int32) :: MaxStemNum= PF_DEFAULT_SOYBEAN_ASIZE

        logical :: determinate
        
        integer(int32)  :: ms_node,br_node(PF_DEFAULT_SOYBEAN_ASIZE),br_from(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_length,br_length(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_width,br_width(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_angle_ave,br_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: ms_angle_sig,br_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)
        

        integer(int32)  :: mr_node,brr_node(PF_DEFAULT_SOYBEAN_ASIZE),brr_from(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_length,brr_length(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_width,brr_width(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_angle_ave,brr_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: mr_angle_sig,brr_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

        real(real64)    :: peti_size_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: peti_size_sig(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: peti_width_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: peti_width_sig(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: peti_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE)
        real(real64)    :: peti_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE)

        real(real64)    :: leaf_angle_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_angle_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_length_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_length_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_width_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_width_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_thickness_ave(PF_DEFAULT_SOYBEAN_ASIZE*3)
        real(real64)    :: leaf_thickness_sig(PF_DEFAULT_SOYBEAN_ASIZE*3)
        
        character(3) :: Stage ! VE, CV, V1,V2, ..., R1, R2, ..., R8
        character(200) :: name
        integer(int32)::stage_id=0
        real(real64) :: dt
        type(Seed_) :: Seed
        type(PlantNode_),allocatable :: NodeSystem(:)
        type(PlantRoot_),allocatable :: RootSystem(:)

        type(Stem_),allocatable :: Stem(:)
        type(Leaf_),allocatable :: Leaf(:)
        type(Root_),allocatable :: Root(:)
        type(Soil_),allocatable :: Soil

        ! material info
        real(real64),allocatable :: stemYoungModulus(:)
        real(real64),allocatable :: leafYoungModulus(:)
        real(real64),allocatable :: rootYoungModulus(:)

        real(real64),allocatable :: stemPoissonRatio(:)
        real(real64),allocatable :: leafPoissonRatio(:)
        real(real64),allocatable :: rootPoissonRatio(:)

        real(real64),allocatable :: stemDensity(:)
        real(real64),allocatable :: leafDensity(:)
        real(real64),allocatable :: rootDensity(:)
        
        ! 節-節点データ構造
        type(Mesh_) :: struct 
        integer(int32),allocatable :: leaf2stem(:,:)
        integer(int32),allocatable :: stem2stem(:,:)
        integer(int32),allocatable :: root2stem(:,:)
        integer(int32),allocatable :: root2root(:,:)
        
        ! 器官オブジェクト配列
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)

        ! シミュレータ
        type(ContactMechanics_) :: contact
        real(real64) :: time
        real(real64) :: seed_length
        real(real64) :: seed_width
        real(real64) :: seed_height
        real(real64),allocatable :: stem_angle(:,:)
        real(real64),allocatable :: root_angle(:,:)
        real(real64),allocatable :: leaf_angle(:,:)

        character(200) :: stemconfig=" "
        character(200) :: rootconfig=" "
        character(200) :: leafconfig=" "

        
        integer(int32),allocatable :: NodeID_MainStem(:)
        type(soybean_NodeID_Branch_),allocatable :: NodeID_Branch(:)

        logical ::  inLoop = .false.
        real(real64) :: hours = 0.0d0

    contains
        procedure,public :: addStem => addStemSoybean
        !procedure,public :: addRoot => addRootSoybean
        !procedure,public :: addLeaf => addLeafSoybean

        ! creation
        procedure,public :: Init => initsoybean
        procedure,public :: remove => removeSoybean
        procedure,public :: create => initsoybean
        procedure,public :: new => initsoybean
        procedure,public :: sowing => initsoybean
        procedure,public :: export => exportSoybean
        procedure,public :: expanition => expanitionSoybean
        procedure,public :: development => developmentSoybean

        ! observation
        procedure,public :: stemlength => stemlengthSoybean
        procedure,public :: NumberOfBranch => NumberOfBranchSoybean
        procedure,public :: isMainStem => isMainStemSoybean   
        procedure,public :: isBranchStem => isBranchStemSoybean     

        ! operation
        procedure,public :: findApical => findApicalSoybean
        
        procedure,public :: grow => growSoybean
        procedure,public :: getVolume => getVolumeSoybean
        procedure,public :: getBioMass => getBioMassSoybean
        procedure,public :: getTotalWeight => getTotalWeightSoybean
        procedure,public :: getSubDomain => getSubDomainSoybean
        procedure,public :: getSubDomainType => getSubDomainTypeSoybean
        procedure,public :: setSubDomain => setSubDomainSoybean
        
        
        procedure,public :: resize => resizeSoybean
        procedure,public :: deform => deformSoybean
        
        ! visualization
        procedure,public :: show => showSoybean
        procedure,public :: gmsh => gmshSoybean
        procedure,public :: msh => mshSoybean
        procedure,public :: vtk => vtkSoybean
        procedure,public :: stl => stlSoybean
        procedure,public :: json => jsonSoybean
        
        ! get info
        !procedure,public :: properties => propertiesSoybean
        ! number of subdomain
        procedure,public :: ns => nsSoybean
        ! number of element
        procedure,public :: ne => neSoybean
        ! number of points
        procedure,public :: nn => nnSoybean
        procedure,public :: np => nnSoybean
        procedure,public :: branchID =>branchIDSoybean

        ! regacy/experimental
        procedure,public :: WaterAbsorption => WaterAbsorptionSoybean
        procedure,public :: move => moveSoybean

        procedure,public :: numleaf => numleafsoybean
        procedure,public :: numstem => numstemsoybean
        procedure,public :: numroot => numrootsoybean

        procedure,public :: laytracing => laytracingsoybean
        procedure,public :: SinkSourceFlow => SinkSourceFlowSoybean

        procedure,public :: update => updateSoybean
        procedure,public :: updateFlowers => updateFlowersSoybean
        procedure,public :: updatePods => updatePodsSoybean
        procedure,public :: AddNode => AddNodeSoybean
    end type

    type :: SoybeanCanopy_
        real(real64) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
    end type

contains

! ########################################
recursive subroutine updateSoybean(obj,stem_id, root_id, leaf_id,debug)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: stem_id, root_id, leaf_id    
    integer(int32) :: i,j,this_stem_id,next_stem_id,A_id,B_id,itr_tol,itr
    integer(int32) :: this_leaf_id,next_leaf_id
    integer(int32) :: this_root_id,next_root_id
    real(real64) :: x_A(3),x_B(3),diff(3),error,last_error,mgn
    logical,optional,intent(in) :: debug
    ! update connectivity
    if(.not. allocated(obj%stem2stem ))then
        print *, "updateSoybean >> ERROR :: .not. allocated(obj%stem2stem )"
        return
    endif


    ! margin between subdomains
    
    itr_tol = 100
    itr=0

    ! if debug
    !if(present(debug) )then
    !    if(debug)then
    !        print *, "obj%stem2stem"
    !        call print(obj%stem2stem)
    !    endif
    !endif

    ! stem to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%stem2stem,1)
            do j=1, size(obj%stem2stem,2)
                this_stem_id = j
                next_stem_id = i
                if(obj%stem2stem(i,j)/=0 .and. i /= j)then
                    ! this_stem_id ===>>> next_stem_id, connected!
                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    x_A(:) = obj%stem(next_stem_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%stem(next_stem_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo

    ! root to root
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%root2root,1)
            do j=1, size(obj%root2root,2)
                this_root_id = j
                next_root_id = i
                if(obj%root2root(i,j)/=0 .and. i /= j)then
                    ! this_root_id ===>>> next_root_id, connected!
                    x_B(:) = obj%root(this_root_id)%getCoordinate("B")
                    x_A(:) = obj%root(next_root_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%root(next_root_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo


    ! leaf to stem
    last_error = 1.0d0
    do 
        itr=itr+1
        error = 0.0d0
        do i=1, size(obj%leaf2stem,1)
            do j=1, size(obj%leaf2stem,2)
                this_stem_id = j
                next_leaf_id = i
                if(obj%leaf2stem(i,j)==1)then
                    ! this_stem_id ===>>> next_leaf_id, connected!
                    x_B(:) = obj%stem(this_stem_id)%getCoordinate("B")
                    x_A(:) = obj%leaf(next_leaf_id)%getCoordinate("A")
                    diff(:) = x_B(:) - x_A(:)
                    error = error + dot_product(diff,diff)
                    call obj%leaf(next_leaf_id)%move(x=diff(1),y=diff(2),z=diff(3) )
                endif
            enddo
        enddo
        if(present(debug) )then
            if(debug)then
                print *, "soybean % update >> error :: ",error
            endif
        endif
        if(itr > itr_tol) then
            print *, "soybean % update >> ERROR :: not converged"
            stop
        endif
        
        if( abs(error) + abs(last_error) == 0.0d0) exit
        last_error = error
    enddo

    
    
end subroutine
! ########################################

! ########################################
subroutine initsoybean(obj,config,&
    regacy,mass,water_content,radius,location,x,y,z,&
    PlantRoot_diameter_per_seed_radius,max_PlantNode_num,Variety,FileName,&
    max_leaf_num,max_stem_num,max_root_num,profiler)
    class(Soybean_),intent(inout) :: obj

    real(real64),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(real64),optional,intent(in) :: PlantRoot_diameter_per_seed_radius
    character(*),optional,intent(in) :: Variety,FileName,config
    logical,optional,intent(in) :: regacy, profiler
    character(200) :: fn,conf,line
    integer(int32),optional,intent(in) :: max_PlantNode_num,max_leaf_num,max_stem_num,max_root_num
    real(real64) :: MaxThickness,Maxwidth,loc(3),vec(3),rot(3),zaxis(3),meshloc(3),meshvec(3)
    integer(int32) :: i,j,k,blcount,id,rmc,n,node_id,node_id2,elemid,branch_id,num_stem_node
    
    real(real64)::readvalreal
    integer(int32) :: readvalint
    logical :: debug=.false.
    logical :: timeOpt = .false.
    type(IO_) :: soyconf
    type(Random_) :: random
    type(Time_) :: time
    type(Stem_) :: stem
    type(Leaf_) :: leaf
    type(Root_) :: root


    timeOpt = input(default=.false.,option=profiler)

    if(timeOpt) then
        call time%start()
    endif


    call obj%remove()
    ! set default parameters
    ! stem
    obj%br_node(:)=0
    obj%br_from(:)=0
    obj%br_length(:)=0.0d0

    obj%br_angle_ave(:)= 0.0d0
    obj%br_angle_sig(:)=10.0d0
    obj%br_angle_ave(1)=30.0d0
    obj%br_angle_sig(1)=2.0d0
    
    obj%ms_angle_ave=0.0d0
    obj%ms_angle_sig=2.0d0

    ! for roots
    obj%brr_node(:)=0
    obj%brr_from(:)=0
    obj%brr_length(:)=0.0d0

    obj%brr_angle_ave(:)= 0.0d0
    obj%brr_angle_sig(:)=10.0d0
    obj%brr_angle_ave(1)=30.0d0
    obj%brr_angle_sig(1)=2.0d0
    
    obj%mr_angle_ave=0.0d0
    obj%mr_angle_sig=2.0d0
    ! peti
    ! is also stem
    
    obj%peti_size_ave(:) = 0.20d0
    obj%peti_size_sig(:) = 0.010d0

    obj%peti_width_ave(:) = 0.0050d0
    obj%peti_width_sig(:) = 0.00010d0

    obj%peti_angle_ave(:) = 30.0d0
    obj%peti_angle_sig(:) = 1.00d0

    ! leaf
    obj%leaf_length_ave(:) = 0.20d0
    obj%leaf_length_sig(:) = 0.01d0

    obj%leaf_width_ave(:) = 0.050d0
    obj%leaf_width_sig(:) = 0.010d0

    obj%leaf_thickness_ave(:) = 0.00100d0
    obj%leaf_thickness_sig(:) = 0.00050d0

    obj%leaf_angle_ave(:) = 40.0d0
    obj%leaf_angle_sig(:) = 10.0d0
    

    
    if(timeOpt) then
        print *, "[1] set default values :: "
        call time%show()
    endif
    

    
    ! 子葉節、初生葉節、根の第1節まで種子の状態で存在

    ! 節を生成するためのスクリプトを開く
    if(.not.present(config).or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        print *, "New soybean-configuration >> soyconfig.json"
        call soyconf%open("soyconfig.json")
        write(soyconf%fh,*) '{'
        write(soyconf%fh,*) '   "type": "soybean",'
        write(soyconf%fh,*) '   "stemconfig": "stemconfig.json",'
        write(soyconf%fh,*) '   "rootconfig": "rootconfig.json",'
        write(soyconf%fh,*) '   "leafconfig": "leafconfig.json",'
        write(soyconf%fh,*) '   "stage": 0,'
        write(soyconf%fh,*) '   "length": 0.0090,'
        write(soyconf%fh,*) '   "width" : 0.0081,'
        write(soyconf%fh,*) '   "height": 0.0072,'
        write(soyconf%fh,*) '   "MaxLeafNum": 50,'
        write(soyconf%fh,*) '   "MaxRootNum":200,'
        write(soyconf%fh,*) '   "MaxStemNum": 50,'

        ! stem
        write(soyconf%fh,*) '   "br_node" : 0,'
        write(soyconf%fh,*) '   "br_from" : 0,'
        write(soyconf%fh,*) '   "br_length" : 0.00,'
        write(soyconf%fh,*) '   "br_angle_ave" : 0.00,'
        write(soyconf%fh,*) '   "br_angle_sig" : 10.00,'
        write(soyconf%fh,*) '   "br_angle_ave(1)": 360.00,'
        write(soyconf%fh,*) '   "br_angle_sig(1)": 2.00,'
        write(soyconf%fh,*) '   "ms_angle_ave": 0.00,'
        write(soyconf%fh,*) '   "ms_angle_sig": 2.00,'


        ! root
        write(soyconf%fh,*) '   "brr_node" : 0,'
        write(soyconf%fh,*) '   "brr_from" : 0,'
        write(soyconf%fh,*) '   "brr_length" : 0.00,'
        write(soyconf%fh,*) '   "brr_angle_ave" : 0.00,'
        write(soyconf%fh,*) '   "brr_angle_sig" : 10.00,'
        write(soyconf%fh,*) '   "brr_angle_ave(1)": 360.00,'
        write(soyconf%fh,*) '   "brr_angle_sig(1)": 2.00,'
        write(soyconf%fh,*) '   "mr_angle_ave": 0.00,'
        write(soyconf%fh,*) '   "mr_angle_sig": 2.00,'
        ! peti
        ! is also stem
        write(soyconf%fh,*) '   "peti_size_ave"  :  0.200,'
        write(soyconf%fh,*) '   "peti_size_sig"  :  0.0100,'
        write(soyconf%fh,*) '   "peti_width_ave"  :  0.00500,'
        write(soyconf%fh,*) '   "peti_width_sig"  :  0.000100,'
        write(soyconf%fh,*) '   "peti_angle_ave"  :  30.00,'
        write(soyconf%fh,*) '   "peti_angle_sig"  :  1.000,'
        ! leaf
        write(soyconf%fh,*) '   "leaf_length_ave"  :  0.200,'
        write(soyconf%fh,*) '   "leaf_length_sig"  :  0.010,'
        write(soyconf%fh,*) '   "leaf_width_ave"  :  0.0500,'
        write(soyconf%fh,*) '   "leaf_width_sig"  :  0.0100,'
        write(soyconf%fh,*) '   "leaf_thickness_ave"  :  0.001000,'
        write(soyconf%fh,*) '   "leaf_thickness_sig"  :  0.000500,'
        write(soyconf%fh,*) '   "leaf_angle_ave"  :  40.00,'
        write(soyconf%fh,*) '   "leaf_angle_sig"  :  10.00'
        write(soyconf%fh,*) '}'
        conf="soyconfig.json"
        call soyconf%close()
    else
        conf = trim(config)
    endif
    

    if(timeOpt) then
        print *, "[2] create default config "
        call time%show()
    endif

    line = soyconf%parse(trim(conf),key1="Genotype",key2="Dt1")
    if(index(line,"Dt1")/=0 )then
        obj%determinate=.False.
    else
        obj%determinate=.True.
    endif

    call soyconf%open(trim(conf))
    blcount=0
    do
        read(soyconf%fh,'(a)') line
        if(debug) print *, trim(line)
        if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"Name")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%name
            endif

            if(index(line,"Mainstem")/=0)then
                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, trim(line)
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_length
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_width
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ms_node
                    endif

                    
                
                enddo
            endif

            if(index(line,"Branch#")/=0)then
                rmc=index(line,"{")
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,':')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,"#")
                if(debug) print *, trim(line)
                read(line(id+1:),*) branch_id

                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, trim(line)
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_length(branch_id)
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_Width(branch_id)
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_node(branch_id)
                    endif

                    if(index(line,"From")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%br_from(branch_id)
                    endif
                
                enddo
            endif

            ! for roots

            if(index(line,"Mainroot")/=0)then
                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, trim(line)
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_length
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_width
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%mr_node
                    endif

                    
                
                enddo
            endif

            if(index(line,"Branchroot#")/=0)then
                rmc=index(line,"{")
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,'"')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                rmc=index(line,':')
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,"#")
                if(debug) print *, trim(line)
                read(line(id+1:),*) branch_id

                do
                    read(soyconf%fh,'(a)') line
                    if(debug) print *, trim(line)
                    if( index(line,"}")/=0 )then
                        exit
                    endif
                    
                    if(index(line,"Length")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_length(branch_id)
                    endif

                    if(index(line,"Width")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_Width(branch_id)
                    endif
                    
                    if(index(line,"Node")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_node(branch_id)
                    endif

                    if(index(line,"From")/=0 )then
                        rmc=index(line,",")
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%brr_from(branch_id)
                    endif
                
                enddo
            endif


            if(index(line,"rootconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%rootconfig
            endif

            if(index(line,"stemconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stemconfig
            endif

            if(index(line,"leafconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%leafconfig
            endif


            if(index(line,"stage")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stage_id
            endif


            if(index(line,"MaxLeafNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxLeafNum
            endif


            if(index(line,"MaxStemNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxStemNum
            endif


            if(index(line,"MaxRootNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxRootNum
            endif

            if(index(line,"length")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_length
            endif

            if(index(line,"width")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_width
            endif

            if(index(line,"height")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_height
            endif


            ! for version 2020.11.24

            ! stem
            if(index(line,"br_angle_ave") /=0 .and. index(line,"br_angle_ave(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"br_angle_sig") /=0 .and. index(line,"br_angle_sig(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_sig(:) = readvalreal
            endif

            if(index(line,"br_angle_ave(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_ave(1) = readvalreal
            endif
            if(index(line,"br_angle_sig(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%br_angle_sig(1) = readvalreal
            endif

            if(index(line,"ms_angle_ave")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%ms_angle_ave = readvalreal
            endif
            
            if(index(line,"ms_angle_sig")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%ms_angle_sig = readvalreal
            endif
            ! peti
            ! is also stem
            
            if(index(line,"peti_size_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_size_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_size_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_size_sig(:) = readvalreal
            endif
            
            if(index(line,"peti_width_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_width_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_width_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_width_sig(:) = readvalreal
            endif
            
            if(index(line,"peti_angle_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"peti_angle_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%peti_angle_sig(:) = readvalreal
            endif
            ! leaf
            
            if(index(line,"leaf_length_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_length_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_length_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_length_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_width_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_width_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_width_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_width_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_thickness_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_thickness_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_thickness_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_thickness_sig(:) = readvalreal
            endif
            
            if(index(line,"leaf_angle_ave")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"leaf_angle_sig")  /=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%leaf_angle_sig(:) = readvalreal
            endif


            ! added in 2020/12/15
            ! for roots



            if(index(line,"brr_angle_ave") /=0 .and. index(line,"brr_angle_ave(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_ave(:) = readvalreal
            endif
            
            if(index(line,"brr_angle_sig") /=0 .and. index(line,"brr_angle_sig(") ==0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_sig(:) = readvalreal
            endif

            if(index(line,"brr_angle_ave(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_ave(1) = readvalreal
            endif
            if(index(line,"brr_angle_sig(1)")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%brr_angle_sig(1) = readvalreal
            endif

            if(index(line,"mr_angle_ave")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%mr_angle_ave = readvalreal
            endif
            
            if(index(line,"mr_angle_sig")/=0 )then
                
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) readvalreal
                obj%mr_angle_sig = readvalreal
            endif


            cycle

        endif

    enddo
    call soyconf%close()

    

    if(index(config,".json")==0 )then
        obj%stemconfig=" "
        obj%rootconfig=" "
        obj%leafconfig=" "
    endif


    if(timeOpt) then
        print *, "[3] read config "
        call time%show()
    endif

    if(obj%ms_node/=0)then
        ! loaded from Mainstem-Branches relation file format
        ! ex.
!       {
!           "Name":"soybean",
!           "Mainstem":{
!               "Length":1.2,
!               "Node":13
!           },
!           "Branch#1":{
!               "From":1,
!               "Length":0.6,
!               "Node":7
!           },
!           "Branch#2":{
!               "From":3,
!               "Length":0.2,
!               "Node":2
!           },
!           "Branch#3":{
!               "From":4,
!               "Length":0.2,
!               "Node":2
!           }
!       }
        ! count number of nodes
        !num_node = countif(obj%ms_node,notEquai=.true.,0)
        !num_node = num_node + countif(obj%br_node,notEquai=.true.,0)
        
        allocate(obj%leaf(obj%MaxLeafNum) )
        allocate(obj%root(obj%MaxrootNum) )
        allocate(obj%stem(obj%MaxstemNum) )

        allocate(obj%leafYoungModulus(obj%MaxLeafNum) )
        allocate(obj%rootYoungModulus(obj%MaxrootNum) )
        allocate(obj%stemYoungModulus(obj%MaxstemNum) )
        ! default value
        obj%leafYoungModulus(:) = 1000.0d0
        obj%rootYoungModulus(:) = 1000.0d0
        obj%stemYoungModulus(:) = 1000.0d0
        
        allocate(obj%leafPoissonRatio(obj%MaxLeafNum) )
        allocate(obj%rootPoissonRatio(obj%MaxrootNum) )
        allocate(obj%stemPoissonRatio(obj%MaxstemNum) )
        obj%leafPoissonRatio(:) = 0.30d0
        obj%rootPoissonRatio(:) = 0.30d0
        obj%stemPoissonRatio(:) = 0.30d0
        
        allocate(obj%leafDensity(obj%MaxLeafNum) )
        allocate(obj%rootDensity(obj%MaxrootNum) )
        allocate(obj%stemDensity(obj%MaxstemNum) )

        obj%leafDensity(:) = 0.0d0
        obj%rootDensity(:) = 0.0d0
        obj%stemDensity(:) = 0.0d0

        allocate(obj%stem2stem(obj%MaxstemNum,obj%MaxstemNum) )
        allocate(obj%leaf2stem(obj%MaxstemNum,obj%MaxLeafNum) )
        allocate(obj%root2stem(obj%MaxrootNum,obj%MaxstemNum) )
        allocate(obj%root2root(obj%MaxrootNum,obj%MaxrootNum) )
        obj%stem2stem(:,:) = 0
        obj%leaf2stem(:,:) = 0
        obj%root2stem(:,:) = 0
        obj%root2root(:,:) = 0

        ! set mainstem
        
        allocate(obj%NodeID_MainStem(obj%ms_node) )
        call stem%init(config=obj%stemconfig)
        do i=1,obj%ms_node

            !call obj%stem(i)%init(config=obj%stemconfig)
            obj%stem(i) = stem
            obj%NodeID_MainStem(i) = i
            call obj%stem(i)%resize(&
                x = obj%ms_width, &
                y = obj%ms_width, &
                z = obj%ms_length/dble(obj%ms_node) &
                )
            call obj%stem(i)%rotate(&
                x = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                y = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                z = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig))   &
                )                
        enddo


        if(timeOpt) then
            print *, "[4] created Main stem."
            call time%show()
        endif
        
        do i=1,obj%ms_node-1
            call obj%stem(i+1)%connect("=>",obj%stem(i))
            obj%stem2stem(i+1,i) = 1
        enddo

        ! set branches
        k=obj%ms_node
        allocate(obj%NodeID_Branch( size(obj%br_node) ) )
        do i=1,size(obj%br_node) ! num branch
            allocate( obj%NodeID_Branch(i)%ID(obj%br_node(i))  )
            do j=1, obj%br_node(i)
                k = k + 1
                !call obj%stem(k)%init(config=obj%stemconfig)
                obj%stem(k) = stem
                obj%NodeID_Branch(i)%ID(j) = k

                call obj%stem(k)%resize(&
                    x = obj%br_width(i), &
                    y = obj%br_width(i), &
                    z = obj%br_length(i)/dble(obj%br_node(i) ) &
                    )
                    
                call obj%stem(k)%rotate(&
                    x = radian(random%gauss(mu=obj%br_angle_ave(j),sigma=obj%br_angle_sig(j) )),  &
                    y = 0.0d0,  &
                    z = radian(360.0d0*random%random() )   &
                    )                
                
                if(j==1)then
                    call obj%stem(k)%connect("=>",obj%stem(obj%br_from(i)  ))
                    obj%stem2stem(k,obj%br_from(i) ) = 1
                else
                    call obj%stem(k)%connect("=>",obj%stem(k-1))
                    obj%stem2stem(k,k-1) = 1
                endif
                    
            enddo
        enddo
        
        if(timeOpt) then
            print *, "[4] created Branches."
            call time%show()
        endif




        ! peti and leaf
        obj%num_stem_node = k
        obj%num_leaf = 0
        ! bugfix 2021/08/18
        call leaf%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
        do i=1, k
            ! ３複葉
            ! add peti
            obj%num_stem_node = obj%num_stem_node +1
            !call obj%stem(obj%num_stem_node)%init(config=obj%stemconfig)
            obj%stem(obj%num_stem_node) = stem
            call obj%stem(obj%num_stem_node)%resize(&
                x = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                y = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                z = random%gauss(mu=obj%peti_size_ave(i),sigma=obj%peti_size_sig(i)) &
                )
            
            call obj%stem(obj%num_stem_node)%rotate(&
                x = radian(random%gauss(mu=obj%peti_angle_ave(i),sigma=obj%peti_angle_sig(i) )),  &
                y = 0.0d0,  &
                z = radian(360.0d0*random%random() )   &
                )      
            call obj%stem(obj%num_stem_node)%connect("=>",obj%stem(i))
            !obj%leaf2stem(num_stem_node,i) = 1   
            obj%stem2stem(obj%num_stem_node,i) = 1            

            

            ! add leaves
            do j=1,3
                obj%num_leaf=obj%num_leaf+1
                !call obj%leaf(obj%num_leaf)%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
                obj%leaf(obj%num_leaf) = leaf
                call obj%leaf(obj%num_leaf)%resize(&
                    y = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  , &
                    z = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) , &
                    x = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i)) &
                )
                call obj%leaf(obj%num_leaf)%rotate(&
                    x = radian(random%gauss(mu=obj%leaf_angle_ave(i),sigma=obj%leaf_angle_sig(i))), &
                    y = 0.0d0, &
                    z = radian(random%random()*360.0d0) &
                )
                call obj%leaf(obj%num_leaf)%connect("=>",obj%stem(obj%num_stem_node))
                obj%leaf2stem(obj%num_leaf,obj%num_stem_node) = 1
            enddo
            
        enddo


        if(timeOpt) then
            print *, "[4] created Peti and Leaves."
            call time%show()
        endif

        ! set mainroot
        call root%init(obj%rootconfig)
        do i=1,obj%mr_node

            obj%root(i) = root
            call obj%root(i)%resize(&
                x = obj%mr_width, &
                y = obj%mr_width, &
                z = obj%mr_length/dble(obj%mr_node) &
                )
            call obj%root(i)%rotate(&
                x = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
                y = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
                z = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig))   &
                )                
        enddo

        do i=1,obj%mr_node-1
            if(i==1)then
                call obj%root(1)%connect("=>",obj%stem(1))    
                obj%root2stem(1,1) = 1
            endif
            call obj%root(i+1)%connect("=>",obj%root(i))
            obj%root2root(i+1,i) = 1
        enddo

        ! set branches
        k=obj%mr_node
        do i=1,size(obj%brr_node)
            do j=1, obj%brr_node(i)
                k = k + 1
                !call obj%root(k)%init(config=obj%rootconfig)
                obj%root(k) = root
                call obj%root(k)%resize(&
                    x = obj%mr_width, &
                    y = obj%mr_width, &
                    z = obj%mr_length/dble(obj%mr_node) &
                    )
                    
                call obj%root(k)%rotate(&
                    x = radian(random%gauss(mu=obj%brr_angle_ave(j),sigma=obj%brr_angle_sig(j) )),  &
                    y = 0.0d0,  &
                    z = radian(360.0d0*random%random() )   &
                    )                
                
                if(j==1)then
                    call obj%root(k)%connect("=>",obj%root(obj%brr_from(i)  ))
                    obj%root2root(k,obj%brr_from(i) ) = 1
                else
                    call obj%root(k)%connect("=>",obj%root(k-1))
                    obj%root2root(k,k-1) = 1
                endif
                    
            enddo
        enddo
        

        obj%stage = "V"//trim(str(obj%ms_node))


        if(timeOpt) then
            print *, "[4] create objects."
        call time%show()
    endif
        return
    else
        ! create leaf, root, stem
        allocate(obj%leaf(obj%MaxLeafNum) )
        allocate(obj%root(obj%MaxrootNum) )
        allocate(obj%stem(obj%MaxstemNum) )

        allocate(obj%leafYoungModulus(obj%MaxLeafNum) )
        allocate(obj%rootYoungModulus(obj%MaxrootNum) )
        allocate(obj%stemYoungModulus(obj%MaxstemNum) )
        ! default value
        obj%leafYoungModulus(:) = 1000.0d0
        obj%rootYoungModulus(:) = 1000.0d0
        obj%stemYoungModulus(:) = 1000.0d0
        
        allocate(obj%leafPoissonRatio(obj%MaxLeafNum) )
        allocate(obj%rootPoissonRatio(obj%MaxrootNum) )
        allocate(obj%stemPoissonRatio(obj%MaxstemNum) )
        obj%leafPoissonRatio(:) = 0.30d0
        obj%rootPoissonRatio(:) = 0.30d0
        obj%stemPoissonRatio(:) = 0.30d0
        
        allocate(obj%leafDensity(obj%MaxLeafNum) )
        allocate(obj%rootDensity(obj%MaxrootNum) )
        allocate(obj%stemDensity(obj%MaxstemNum) )

        obj%leafDensity(:) = 0.0d0
        obj%rootDensity(:) = 0.0d0
        obj%stemDensity(:) = 0.0d0

        allocate(obj%stem2stem(obj%MaxstemNum,obj%MaxstemNum) )
        allocate(obj%leaf2stem(obj%MaxstemNum,obj%MaxLeafNum) )
        allocate(obj%root2stem(obj%MaxrootNum,obj%MaxstemNum) )
        allocate(obj%root2root(obj%MaxrootNum,obj%MaxrootNum) )
        
        !allocate(obj%struct%NodCoord(4,3) )
        !allocate(obj%struct%ElemNod(3,2) )
        !allocate(obj%struct%ElemMat(3) )
        ! 子葉結節部=(0,0,0)
        !obj%struct%NodCoord(1,1:3) = 0.0d0
        call obj%leaf(1)%init(obj%leafconfig,species=PF_GLYCINE_SOJA)
        call obj%leaf(1)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(10.0d0) )
        call obj%leaf(2)%init(obj%leafconfig,species=PF_GLYCINE_SOJA)
        call obj%leaf(2)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(-10.0d0) )
        
        call obj%stem(1)%init(obj%stemconfig)
        call obj%stem(1)%rotate(x=radian(40.0d0) )
        
        call obj%stem(2)%init(obj%stemconfig)
        call obj%stem(2)%rotate(x=radian(80.0d0) )
    
        call obj%root(1)%init(obj%rootconfig)
        call obj%root(1)%fix(x=0.0d0,y=0.0d0,z=0.0d0)
        call obj%root(1)%rotate(x=radian(-60.0d0) )
    
        call obj%leaf(1)%connect("=>",obj%stem(1))
        obj%leaf2stem(1,1) = 1
        
        call obj%leaf(2)%connect("=>",obj%stem(1))
        obj%leaf2stem(2,1) = 1
        
        call obj%stem(2)%connect("=>",obj%stem(1))
        obj%stem2stem(2,1) = 1
        
        call obj%root(1)%connect("=>",obj%stem(1))
        obj%root2stem(1,1) = 1
        
        obj%stage = "VE"
        ! 初生葉結節部
        !obj%struct%NodCoord(2,1) = 0.0d0
        !obj%struct%NodCoord(2,2) = 0.0d0
        !obj%struct%NodCoord(2,3) = 1.0d0/20.0d0*obj%seed_height
        ! 地際部
        !obj%struct%NodCoord(3,1) = 1.0d0/4.0d0*obj%seed_length
        !obj%struct%NodCoord(3,2) = 0.0d0
        !obj%struct%NodCoord(3,3) = -1.0d0/3.0d0*obj%seed_height
        ! 根冠
        !obj%struct%NodCoord(4,1) = 1.0d0/2.0d0*obj%seed_length
        !obj%struct%NodCoord(4,2) = 0.0d0
        !obj%struct%NodCoord(4,3) = -1.0d0/2.0d0*obj%seed_height
    
        ! 子葉-初生葉節
        !obj%struct%ElemNod(1,1) = 1
        !obj%struct%ElemNod(1,2) = 2
        ! 地際-子葉節
        !obj%struct%ElemNod(2,1) = 3
        !obj%struct%ElemNod(2,2) = 1
        ! 地際-根冠節
        !obj%struct%ElemNod(3,1) = 3
        !obj%struct%ElemNod(3,2) = 4
    
        ! 子葉-初生葉節 stem: 1
        !obj%struct%ElemMat(1) = 1
        ! 地際-子葉節 stem: 1
        !obj%struct%ElemMat(2) = 1
        ! 地際-根冠節 primary root: -1
        !obj%struct%ElemMat(3) = -1
    
        ! FEメッシュを生成
        ! 領域を確保
    !    n = input(default=80,option=max_leaf_num)
    !    allocate(obj%leaf_list(n) )
    !    n = input(default=80,option=max_stem_num)
    !    allocate(obj%stem_list(n) )
    !    n = input(default=80,option=max_root_num)
    !    allocate(obj%root_list(n) )
    !
    !    ! 子葉のメッシュを生成
    !    call obj%leaf_list(1)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
    !    call obj%leaf_list(1)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
    !
    !    call obj%leaf_list(2)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
    !    call obj%leaf_list(2)%rotate(x=radian(180.0d0) )
    !    call obj%leaf_list(2)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
    !
    !
    !
    !    ! 子葉-初生葉節のメッシュを生成
    !    rot(:) = 0.0d0
    !    call obj%stem_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(1,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(1,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    
    !    ! structの構造データにメッシュデータを合わせる。
    !    print *, obj%stem_list(1)%Mesh%BottomElemID
    !    print *, obj%stem_list(1)%Mesh%TopElemID
    !
    !    elemid = obj%stem_list(1)%Mesh%BottomElemID
    !    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
    !    meshloc(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)
    !
    !    elemid = obj%stem_list(1)%Mesh%TopElemID
    !    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
    !    meshvec(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)-meshloc(:)
    
        !print *, "loc",loc
        !print *, "meshloc",meshloc
        !print *, "vec",vec
        !print *, "meshvec",meshvec
        
    !    ! 節中央を原点へ
    !    call obj%stem_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0)
    !    
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    call obj%stem_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    call obj%stem_list(1)%rotate(x=0.0d0,y=0.0d0,z=0.0d0 )
    !!    
    !    
    !!    
    !
    !
    !    ! 地際-子葉節のメッシュを生成
    !    rot(:) = 0.0d0
    !    call obj%stem_list(2)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(2,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(2,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    ! 節中央を原点へ
    !    call obj%stem_list(2)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
    !        z=-obj%seed_length/8.0d0)
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot
    !    !call obj%stem_list(2)%rotate(x=rot(1),y=rot(2),z=rot(3) )
    !    call obj%stem_list(2)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    
    !
    !
    !    ! 地際-根冠節のメッシュ生成
    !    rot(:) = 0.0d0
    !    call obj%root_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
    !        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
    !    ! 節基部の節点ID
    !    node_id = obj%struct%ElemNod(3,1)
    !    ! 節先端部の節点ID
    !    node_id2= obj%struct%ElemNod(3,2)
    !    ! 節基部の位置ベクトル
    !    loc(:) = obj%struct%NodCoord( node_id  ,:)
    !    ! 節先端部までの方向ベクトル
    !    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
    !    ! 節基部へ移動
    !    call obj%root_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
    !        z=-obj%seed_length/8.0d0)
    !    call obj%root_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
    !    zaxis(:)=0.0d0
    !    zaxis(3)=obj%seed_length/5.0d0
    !    rot(:) = angles(zaxis,vec)
    !    !call obj%root_list(1)%rotate(x=rot(1),y=rot(2),z=rot(3) )
    !    print *, "loc",loc
    !    print *, "vec",vec
    !    print *, "rot",rot    
    endif




    ! ここからレガシーモード
    if(present(regacy) )then
        if(regacy .eqv. .true.)then
            obj%Stage = "VE"
            if(present(FileName) )then
                fn=FileName
            else
                fn="untitled"
            endif

            loc(:)=0.0d0

            if(present(x) )then
                loc(1)=x
            endif

            if(present(y) )then
                loc(2)=y
            endif

            if(present(z) )then
                loc(3)=z
            endif

            if(present(location) )then
                loc(:)=location(:)    
            endif

            ! initialize RootSystem and NodeSystem
            if(.not.allocated( obj%RootSystem) )then
                allocate(obj%RootSystem( input(default=1000,option=max_PlantNode_num) ) ) 
                obj%num_of_root=1
            endif
            if(.not.allocated( obj%NodeSystem) )then
                allocate(obj%NodeSystem( input(default=1000,option=max_PlantNode_num) ) ) 
                obj%num_of_node=1
            endif

            ! setup seed
            if(Variety=="Tachinagaha" .or. Variety=="tachinagaha" )then
                call obj%Seed%init(mass=mass,width1=9.70d0,width2=8.20d0,&
                    width3=7.70d0,&
                    water_content=water_content,radius=radius,location=loc)    
                call obj%Seed%createMesh(FileName=trim(fn)//".stl",&
                ElemType="Tetrahedra")

                call obj%Seed%convertMeshType(Option="TetraToHexa")

            else
                print *, "Variety name :: is not implemented."
                stop
            endif


            ! setup primary node (plumule)
            call obj%NodeSystem(1)%init(Stage=obj%Stage,&
            Plantname="soybean",location=loc)

            ! setup primary node (radicle))
            MaxThickness=input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            Maxwidth    =input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            call obj%RootSystem(1)%init(Plantname="soybean",&
            Stage=obj%Stage,MaxThickness=MaxThickness,Maxwidth=Maxwidth,location=loc)

            obj%time=0.0d0
            return
        endif
    endif


end subroutine
! ########################################

! ########################################
subroutine growSoybean(obj,dt,light,air,temp,simple)
    class(Soybean_),intent(inout) :: obj
    type(Light_),optional,intent(inout) :: light
    type(air_),optional,intent(in) :: air
    real(real64),optional,intent(in) :: temp
    real(real64),intent(in) :: dt! time-interval
    real(real64) :: ac_temp ! time-interval
    integer(int32) :: i
    logical,optional,intent(in) :: simple
    integer(int32),allocatable :: apicals(:)
    integer(int32),allocatable :: last_apicals(:)
    integer(int32),allocatable :: last_last_apicals(:)


    obj%dt = dt

    if(present(simple) )then
        if(simple)then
            ! simple algorithmic growth
            ! growth by temp by time
            apicals = obj%findApical()
            do i=1,size(apicals)
                ! add stem&leaf
                call obj%addNode(StemNodeID=apicals(i) )
            enddo
            call obj%update()
            return
        endif
    endif

    ! 光量子量を計算
    call obj%laytracing(light=light)

    ! 光合成量を計算
    do i=1,size(obj%Leaf)
        if(obj%Leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%photosynthesis(dt=dt,air=air)
        endif
    enddo

    ! シンクソース輸送を計算
    !call obj%SinkSourceFlow()

    ! ソースの消耗、拡散を計算
    !call obj%source2sink()

    ! 伸長を計算
    !call obj%extention()

    ! 分化を計算、構造の更新
    !call obj%development()

    !限界日長以下>> 花成 & 子実成長
    !if( obj%DayLengthIsShort() .eqv. .true. )then
        !call soybean%updateFlowers()
        !call soybean%updatePods()
    !endif


end subroutine
! ########################################

! ########################################
subroutine SinkSourceFlowSoybean(obj,simple)
    class(Soybean_),intent(inout) :: obj
    logical,optional,intent(in) :: simple
    !type(DiffusionEq_) :: DiffusionEq

    if(present(simple) )then
        if(simple)then
            ! simple flow
            ! for each stem,
            
            return
        endif
    endif
    !call obj%lossEnergy()

    ! solve diffusion equation for multi-domains
    !call DiffusionEq%init(multiDomain=.true.,connectivity=obj%domainConnectivity)
    !call DiffusionEq%add(domainlist=obj%stem(:)%femdomain)
    !call DiffusionEq%add(domainlist=obj%leaf(:)%femdomain)
    !call DiffusionEq%add(domainlist=obj%root(:)%femdomain)
    !call DiffusionEq%FixValue(&
        !range=soil%femdonain, projection=true, values=soil%watercontent)
    
    !call DiffusionEq%run(dt=obj%dt)
    !soybean%sourceContent = Diffusion%unknowns

    
end subroutine
! ########################################


! ########################################
subroutine expanitionSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    !type(ContactMechanics_) :: contact

    !contact%init(connectivity=obj%connectivity)
    !contact%add(domain=obj%stem(:)%domain)
    !contact%add(domain=obj%leaf(:)%domain)
    !contact%add(domain=obj%root(:)%domain)
    !contact%add(domain=soil)
    !contact%Density = obj%density()
    !contact%PoissonRatio = obj%PoissonRatio()
    !contact%PenaltyParameter = obj%PenaltyParameter()
    !contact%fix(bottom=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
    !contact%fix(side=.true., direction="xyz",displacement=[0.0d0,0.0d0,0.0d0] )
    !contact%solve(dt=obj%dt)
    !soybean%CauchyStress = contact%CauchyStress
    !soybean%displacement = contact%displacement

end subroutine
! ########################################


! ########################################
subroutine developmentSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i, new_stem_id,new_leaf_id,new_root_id
    
    !do i=1, obj%numStemApical
    !   stemID=obj%StemApical(i) 
    !   if(obj%stem(stemID)%source => obj%minimalSource )then
    !       new_stem_id = obj%newStem(from=StemID,how=obj%stem(stemID)%properties)
    !       new_leaf_id = obj%newLeaf(from=new_stem_id,how=obj%stem(stemID)%properties)
    !   endif
    !enddo

    !do i=1, obj%numleaf
    !   leafID=i
    !   call obj%leaf(leafID)%grow()
    !enddo

    !do i=1, obj%numrootApical
    !   rootID=obj%rootApical(i) 
    !   if(obj%root(rootID)%source => obj%minimalSource )then
    !       new_root_id = obj%newroot(from=rootID,how=obj%root(rootID)%properties)
    !   endif
    !enddo

end subroutine
! ########################################



! ########################################
subroutine updateFlowersSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i
    
    !do i=1, obj%numStem() 
    !   call obj%stem(i)%updateFlowerCapacity()
    !enddo

end subroutine
! ########################################


! ########################################
subroutine updatePodsSoybean(obj)
    class(Soybean_),intent(inout) :: obj
    integer(int32) :: i
    
    !do i=1, obj%numStem() 
    !   call obj%stem(i)%updatePodCapacity()
    !   call obj%stem(i)%growPod()
    !enddo

end subroutine
! ########################################





! ########################################
subroutine WaterAbsorptionSoybean(obj,temp,dt)
    class(Soybean_),intent(inout) :: obj
    real(real64),intent(in) :: temp,dt
    real(real64) :: a,b,c,d,AA,BB,w1max,w2max,w3max,time
    real(real64) :: x_rate,y_rate,z_rate,wx,wy,wz

    obj%time=obj%time+dt


    ! tested by tachinagaha, 2019
    a=0.00910d0
    b=-1.76450d0
    c=3.32E-04	
    d=-0.0905180d0
    AA=a*temp+b
    !BB=c*exp(d*temp)
    BB=c*temp+d
    ! width1 becomes 1.7 times, width2 becomes 1.2, width3 becomes 1.1
    w1max=1.70d0
    w2max=1.20d0
    w3max=1.10d0
    obj%seed%width1=obj%seed%width1_origin*(w1max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width2=obj%seed%width2_origin*(w2max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width3=obj%seed%width3_origin*(w3max - AA*exp(-BB*obj%time)   ) 

    ! linear model; it should be changed in near future.
    if(obj%time > 60.0d0*6.0d0)then
        obj%seed%width2=obj%seed%width2_origin*(w2max ) 
        obj%seed%width3=obj%seed%width3_origin*(w3max ) 
    else
        obj%seed%width2=obj%seed%width2_origin + obj%seed%width2_origin*(w2max-1.0d0 )*(obj%time)/(60.0d0*6.0d0) 
        obj%seed%width3=obj%seed%width3_origin + obj%seed%width3_origin*(w3max-1.0d0 )*(obj%time)/(60.0d0*6.0d0)
    endif

    wx = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1)) 
    wy = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2)) 
    wz = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3)) 
    print *, wx,wy,wz
    x_rate =  1.0d0/wx
    y_rate =  1.0d0/wy
    z_rate =  1.0d0/wz
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)
    x_rate = obj%seed%width1
    y_rate = obj%seed%width2
    z_rate = obj%seed%width3
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)


end subroutine
! ########################################


! ########################################
subroutine exportSoybean(obj,FilePath,FileName,SeedID,withSTL,withMesh)
    class(Soybean_),intent(inout) :: obj
    character(*),optional,intent(in) :: FilePath
    character(*),intent(in) :: FileName
    integer(int32),optional,intent(inout) :: SeedID
    logical,optional,intent(in) :: withSTL,withMesh
    integer(int32) :: i,itr

    itr=SeedID
    ! if seed exists => output
    if(obj%Seed%num_of_seed>=0)then
        if(present(withSTL) )then
            if(withSTL .eqv. .true.)then
                call obj%Seed%export(FileName=trim(FileName),SeedID=itr,extention=".stl")    
            endif
        endif
        if(present(withMesh) )then
            if(withMesh .eqv. .true.)then
                call obj%Seed%export(FileName=trim(FileName),SeedID=itr,extention=".pos")    
            endif
        endif

            
        if(present(FilePath) )then
            call obj%Seed%export(FileName=trim(FilePath)//"/seed.geo",SeedID=itr)
        else
            call obj%Seed%export(FileName=trim(FileName),SeedID=itr)
        endif
    endif

    itr=itr+1
    ! export NodeSystem
    do i=1,size(obj%NodeSystem)
            
        if(present(FilePath) )then
            call obj%NodeSystem(i)%export(FileName=trim(FilePath)//"/Node.geo",objID=itr)
        else
            call obj%NodeSystem(i)%export(FileName=trim(FileName)//"_Node.geo",objID=itr)
        endif
        if(i==obj%num_of_node  )then
            exit
        endif
    enddo

    
    ! export RootSystem
    do i=1,size(obj%RootSystem)
            
        if(present(FilePath) )then
            call obj%RootSystem(i)%export(FileName=trim(FilePath)//"/Root.geo",RootID=itr)
        else
            call obj%RootSystem(i)%export(FileName=trim(FileName)//"_Root.geo",RootID=itr)
        endif
        if(i==obj%num_of_root  )then
            exit
        endif
    enddo
    SeedID=itr




end subroutine
! ########################################



! ########################################

! ########################################
!subroutine initsoybean(obj,growth_habit,Max_Num_of_Node)
!    class(soybean_) :: obj
!    character(*),optional,intent(in) :: growth_habit
!    integer(int32),optional,intent(in)::Max_Num_of_Node
!    integer(int32) ::n
!
!    if(present(growth_habit) )then
!        obj%growth_habit=growth_habit
!    else
!        obj%growth_habit="determinate"
!    endif
!
!    obj%growth_stage="VE"
!
!    n=input(default=100,option=Max_Num_of_Node)
!
!    allocate(obj%NodeSystem(n))
!    obj%NumOfNode=0
!    obj%NumOfRoot=0
!
!    ! set an initial node and root
!    ! two leaves, one root.
!
!    call obj%AddNode()
!
!end subroutine
!! ########################################
!
!
!
!
!
!
!! ########################################
!subroutine AddNodeSoybean(obj,SizeRatio)
!    class(soybean_),intent(inout)::obj
!    real(real64),optional,intent(in)::SizeRatio
!    real(real64) :: magnif
!
!    magnif=input(default=1.0d0,option=SizeRatio)
!    obj%NumOfNode=obj%NumOfNode+1
!    
!    ! add leaves
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(2) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    else        
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(3) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add stem
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Stem(1) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add Peti
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Peti(1) )
!        call obj%NodeSystem(obj%NumOfNode)%Peti(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!end subroutine
!! ########################################
!

! ########################################
subroutine showSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in)::name

    if( obj%struct%empty() .eqv. .true.)then
        print *, "Error :: showSoybean>> no structure is imported."
        return
    endif

    call obj%struct%export(name=name)

end subroutine
! ########################################



! ########################################
function numleafsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numstemsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numrootsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################


! ########################################
subroutine gmshSoybean(obj,name,num_threads)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    n = input(default=1,option=num_threads)
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 

    do i=1,size(obj%stem)
        !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%gmsh(name=trim(name)//"_stem"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 

    do i=1,size(obj%root)
        !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%gmsh(name=trim(name)//"_root"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%gmsh(name=trim(name)//"_leaf"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel


end subroutine
! ########################################


! ########################################
subroutine mshSoybean(obj,name,num_threads)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n
    type(IO_) :: f
    ! index file
    call f%open(trim(name)//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_stem"//trim(str(i))//".msh")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_root"//trim(str(i))//".msh")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_leaf"//trim(str(i))//".msh")
            endif
        enddo
    endif
    call f%close()
    
    n = input(default=1,option=num_threads)
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%msh(name=trim(name)//"_stem"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%msh(name=trim(name)//"_root"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%msh(name=trim(name)//"_leaf"//trim(str(i)))
        !endif
    enddo
    !$OMP end do
    !$OMP end parallel

end subroutine
! ########################################


! ########################################
subroutine vtkSoybean(obj,name,num_threads)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    type(IO_) :: f
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i, n

    n = input(default=1,option=num_threads)
    
    ! index file
    call f%open(trim(name)//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_stem"//trim(str(i))//".vtk")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_root"//trim(str(i))//".vtk")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_leaf"//trim(str(i))//".vtk")
            endif
        enddo
    endif
    call f%close()

    if(allocated(obj%stem) )then
        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%stem)
            !if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%stem(i)%vtk(name=trim(name)//"_stem"//trim(str(i)))
            !endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif


    if(allocated(obj%root))then

        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%root)
            !if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%root(i)%vtk(name=trim(name)//"_root"//trim(str(i)))
            !endif
        enddo

        !$OMP end do
        !$OMP end parallel
    endif


    if(allocated(obj%leaf))then

        !$OMP parallel num_threads(n) private(i)
        !$OMP do 
        do i=1,size(obj%leaf)
            !if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
                call obj%leaf(i)%vtk(name=trim(name)//"_leaf"//trim(str(i)))
            !endif
        enddo
        !$OMP end do
        !$OMP end parallel
    endif

    

end subroutine
! ########################################


! ########################################
subroutine jsonSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32) :: i,countnum
    type(IO_) :: f

    call f%open(trim(name)//".json")
    call f%write("{")
    countnum=0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"stem"//trim(str(i))//'":')
            call obj%stem(i)%femdomain%json(name=trim(name)//"_stem"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_stem":'//str(countnum)//',' )

    countnum=0
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"root"//trim(str(i))//'":')
            call obj%root(i)%femdomain%json(name=trim(name)//"_root"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"num_root":'//str(countnum)//',' )
    
    countnum=0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            countnum=countnum+1
            call f%write('"'//"leaf"//trim(str(i))//'":')
            call obj%leaf(i)%femdomain%json(name=trim(name)//"_leaf"//trim(str(i)),fh=f%fh,endl=.false.)
        endif
    enddo
    call f%write('"obj%num_leaf":'//str(countnum)//',' )
    call f%write('"return_soybean":0')
    call f%write("}")
    call f%close()
end subroutine
! ########################################

! ########################################
subroutine stlSoybean(obj,name,num_threads)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32),optional,intent(in) :: num_threads
    integer(int32) :: i,n

    type(IO_) :: f
    ! index file
    call f%open(trim(name)//"_index.txt","w")

    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_stem"//trim(str(i))//".stl")
            endif
        enddo
    endif
    
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_root"//trim(str(i))//".stl")
            endif
        enddo
    endif

    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                call f%write(trim(name)//"_leaf"//trim(str(i))//".stl")
            endif
        enddo
    endif
    call f%close()
    
    n = input(default=1,option=num_threads)
    !call execute_command_line("echo ' ' > "//trim(name)//".stl")
    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%stl(name=trim(name)//"_stem"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_stem"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%stl(name=trim(name)//"_root"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_root"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    !$OMP parallel num_threads(n) private(i)
    !$OMP do 
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%stl(name=trim(name)//"_leaf"//trim(str(i)))
            !call execute_command_line("cat "//trim(name)//"_leaf"//trim(str(i))//"_000001.stl >> "//trim(name)//".stl")
        endif
    enddo
    !$OMP end do
    !$OMP end parallel

    call execute_command_line("cat "//trim(name)//"*_leaf*.stl > "//trim(name)//"_leaf.stl" )
    call execute_command_line("cat "//trim(name)//"*_stem*.stl > "//trim(name)//"_stem.stl" )
    call execute_command_line("cat "//trim(name)//"*_root*.stl > "//trim(name)//"_root.stl" )
    call execute_command_line("cat "//trim(name)//"_leaf.stl "//trim(name)//"_stem.stl "&
        //trim(name)//"_root.stl > "//trim(name)//".stl" )

end subroutine
! ########################################

! ########################################
subroutine moveSoybean(obj,x,y,z)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%move(x=x,y=y,z=z)
        endif
    enddo

end subroutine
! ########################################

! ########################################
subroutine laytracingsoybean(obj,light)
    class(Soybean_),intent(inout) :: obj
    type(Light_),intent(in) :: light
    real(real64),allocatable :: stemcenter(:,:),stemradius(:)
    real(real64),allocatable :: leafcenter(:,:),leafradius(:)
    real(real64),allocatable :: elemnodcoord(:,:),x(:),x2(:)
    real(real64) :: max_PPFD,r,rc,r0
    real(real64),parameter :: extinction_ratio = 100.0d0 ! ratio/m
    !real(real64),parameter :: radius_ratio = 0.01d0 ! radius_of_gauss_point/element_length
    type(IO_) :: f
    integer(int32) :: i,j,n,num_particle,k,l,nodeid,m,totcount

    max_PPFD = light%maxPPFD
    ! 総当りで、総遮蔽長を割り出す
    ! 茎は光を通さない、葉は透過率あり、空間は透過率ゼロ
    ! 要素中心から頂点への平均長さを半径に持ち、要素中心を中心とする球
    ! を考え、Layとの公差判定を行う。
    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%leaf(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(leafcenter(num_particle,3),leafradius(num_particle) )
    leafcenter(:,:) = 0.0d0
    leafradius(:) = 0.0d0

    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%stem(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(stemcenter(num_particle,3),stemradius(num_particle) )
    stemcenter(:,:) = 0.0d0
    stemradius(:) = 0.0d0

    num_particle = 0
    
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%leaf(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%leaf(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%leaf(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%leaf(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%leaf(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%leaf(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        leafcenter(num_particle,l) = &
                        + leafcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - leafcenter(num_particle,:)
                    if(k>=2 .and. leafradius(num_particle) > sqrt(dot_product(x,x))  )then
                        leafradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        leafradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo


    num_particle = 0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%stem(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%stem(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%stem(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%stem(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%stem(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%stem(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        stemcenter(num_particle,l) = &
                        + stemcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - stemcenter(num_particle,:)
                    !最小半径で考える
                    if(k>=2 .and. stemradius(num_particle) > sqrt(dot_product(x,x))  )then
                        stemradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        stemradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo
    

    ! DEBUG
    call f%open("leaf.txt")
    do i=1,size(leafcenter,1)
        write(f%fh,*) leafcenter(i,:)
    enddo
    call f%close()
    
    call f%open("stem.txt")
    do i=1,size(stemcenter,1)
        write(f%fh,*) stemcenter(i,:)
    enddo
    call f%close()
    
    allocate(x(3),x2(3) )
    
    
    num_particle = 0
    totcount = 0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            obj%leaf(i)%PPFD(:) = max_PPFD
            do j=1,size(obj%leaf(i)%PPFD)
                totcount = totcount + 1
                num_particle = num_particle + 1
                ! それぞれの要素について、遮蔽particleを探索
                ! 茎：全減衰
                ! 葉：半減衰
                ! 簡単のため上からのみ
                ! x-yのみについて見て、上方かつx-y平面距離が半径以内で覆陰判定
                x(:) = leafcenter(num_particle,:)
                r0   = leafradius(num_particle)
                ! 枝による覆陰判定
                
                do k=1, size(stemcenter,1)
                    x2(:) = stemcenter(k,:)
                    r     = stemradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= r0 + r .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = 0.0d0
                        exit
                    endif
                enddo
                if(obj%leaf(i)%PPFD(j) == 0.0d0)then
                    cycle
                endif

                do k=1, size(leafcenter,1)
                    ! もし自信だったら除外
                    if(totcount == k)then
                        cycle
                    endif
                    
                    x2(:) = leafcenter(k,:)
                    r     = leafradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= (r0 + r)/2.0d0 .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = &
                        obj%leaf(i)%PPFD(j)*(1.0d0-extinction_ratio*2.0d0*r)
                        if( obj%leaf(i)%PPFD(j) <= 0.0d0 )then
                            obj%leaf(i)%PPFD(j) = 0.0d0
                        endif
                    endif
                enddo

            enddo
        endif
    enddo
    
    call f%open("PPFD.txt")
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            do j=1,size(obj%leaf(i)%PPFD,1)
                write(f%fh,*) obj%leaf(i)%PPFD(j),"leaf_id: ",str(i),"elem_id: ",str(j)
            enddo
        endif
    enddo
    call f%close()
    


end subroutine
! ########################################

subroutine addNodeSoybean(obj,StemNodeID,RootNodeID,peti_width_ave,peti_width_sig,peti_size_ave &
    ,peti_size_sig,peti_angle_ave,peti_angle_sig,leaf_thickness_ave,leaf_thickness_sig &
    ,leaf_length_ave,leaf_length_sig,leaf_width_ave,leaf_width_sig,leaf_angle_sig &
    ,leaf_angle_ave)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: StemNodeID,RootNodeID
    real(real64),optional,intent(in) :: peti_width_ave,peti_width_sig,peti_size_ave &
    ,peti_size_sig,peti_angle_ave,peti_angle_sig,leaf_thickness_ave,leaf_thickness_sig &
    ,leaf_length_ave,leaf_length_sig,leaf_width_ave,leaf_width_sig,leaf_angle_sig &
    ,leaf_angle_ave
    type(Random_) :: random

    integer(int32) :: i,j,branch_id

    if(present(StemNodeID) )then
        i = StemNodeID
        
        obj%leaf_thickness_ave(obj%num_leaf)=input(&
            default=obj%leaf_thickness_ave(obj%num_leaf),&
            option=leaf_thickness_ave)
        obj%leaf_thickness_sig(obj%num_leaf)=input(&
            default=obj%leaf_thickness_sig(obj%num_leaf),&
            option=leaf_thickness_sig)
        
        obj%leaf_length_ave(obj%num_leaf)=input(&
            default=obj%leaf_length_ave(obj%num_leaf),&
            option=leaf_length_ave)
        obj%leaf_length_sig(obj%num_leaf)=input(&
            default=obj%leaf_length_sig(obj%num_leaf),&
            option=leaf_length_sig)
        obj%leaf_width_ave(obj%num_leaf)=input(&
            default=obj%leaf_width_ave(obj%num_leaf),&
            option=leaf_width_ave)
        obj%leaf_width_sig(obj%num_leaf)=input(&
            default=obj%leaf_width_sig(obj%num_leaf),&
            option=leaf_width_sig)
        obj%leaf_angle_sig(obj%num_leaf)=input(&
            default=obj%leaf_angle_sig(obj%num_leaf),&
            option=leaf_angle_sig)
        
        obj%leaf_angle_ave(obj%num_leaf)=input(&
            default=obj%leaf_angle_ave(obj%num_leaf),&
            option=leaf_angle_ave)

        

        if(obj%isMainStem(StemNodeID) )then
            ! main stem
            i = StemNodeID
            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)
            
            call extend(obj%NodeID_MainStem)
            obj%NodeID_MainStem( size(obj%NodeID_MainStem) ) = obj%numStem()
            
            call obj%stem(i)%resize(&
                x = obj%ms_width, &
                y = obj%ms_width, &
                z = obj%ms_length/dble(obj%ms_node) &
                )
            call obj%stem(i)%rotate(&
                x = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                y = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig)),  &
                z = radian(random%gauss(mu=obj%ms_angle_ave,sigma=obj%ms_angle_sig))   &
                )                
        else
            ! branch
            i = StemNodeID
            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)
            branch_id = obj%branchID(i)
            call extend(obj%NodeID_Branch(branch_id)%ID)
            obj%NodeID_Branch(branch_id)%ID( size(obj%NodeID_Branch(branch_id)%ID) ) = obj%numStem()

            call obj%stem(i)%resize(&
                x = obj%br_width(branch_id), &
                y = obj%br_width(branch_id), &
                z = obj%br_length(branch_id)/dble(obj%br_node(branch_id) ) & 
                )
            call obj%stem(i)%rotate(&
                x = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
                y = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) )),  &
                z = radian(random%gauss(mu=obj%br_angle_ave(branch_id),sigma=obj%br_angle_sig(branch_id) ))   &
                )                
        endif
        
        call obj%stem( obj%numStem() )%connect("=>",obj%stem(StemNodeID))
        obj%stem2stem( obj%numStem() , StemNodeID ) = 1

        ! add leaves
        do j=1,3

            call obj%stem(obj%numStem()+1 )%init(config=obj%stemconfig)

            call obj%stem(obj%numStem() )%resize(&
                x = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                y = random%gauss(mu=obj%peti_width_ave(i),sigma=obj%peti_width_sig(i)), &
                z = random%gauss(mu=obj%peti_size_ave(i),sigma=obj%peti_size_sig(i)) &
                )
            
            call obj%stem(obj%numStem() )%rotate(&
                x = radian(random%gauss(mu=obj%peti_angle_ave(i),sigma=obj%peti_angle_sig(i) )),  &
                y = 0.0d0,  &
                z = radian(360.0d0*random%random() )   &
                )      
            call obj%stem(obj%numStem() )%connect("=>",obj%stem(i))
            obj%stem2stem(obj%numStem() ,i) = 1         

            obj%num_leaf=obj%num_leaf+1
            call obj%leaf(obj%num_leaf)%init(config=obj%leafconfig,species=PF_GLYCINE_SOJA)
            call obj%leaf(obj%num_leaf)%resize(&
                y = random%gauss(mu=obj%leaf_thickness_ave(i),sigma=obj%leaf_thickness_sig(i))  , &
                z = random%gauss(mu=obj%leaf_length_ave(i)   ,sigma=obj%leaf_length_sig(i)) , &
                x = random%gauss(mu=obj%leaf_width_ave(i)    ,sigma=obj%leaf_width_sig(i)) &
            )
            call obj%leaf(obj%num_leaf)%rotate(&
                x = radian(random%gauss(mu=obj%leaf_angle_ave(i),sigma=obj%leaf_angle_sig(i))), &
                y = 0.0d0, &
                z = radian(random%random()*360.0d0) &
            )
            call obj%leaf(obj%num_leaf)%connect("=>",obj%stem(obj%numStem() ))
            obj%leaf2stem(obj%num_leaf,obj%numStem() ) = 1

        enddo
    elseif(present(RootNodeID) )then

        ! set mainroot
        call obj%root(obj%numRoot()+1 )%init(obj%rootconfig)
        call obj%root(i)%resize(&
            x = obj%mr_width, &
            y = obj%mr_width, &
            z = obj%mr_length/dble(obj%mr_node) &
            )
        call obj%root(i)%rotate(&
            x = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
            y = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig)),  &
            z = radian(random%gauss(mu=obj%mr_angle_ave,sigma=obj%mr_angle_sig))   &
            )                
        
        i = RootNodeID
        call obj%root(obj%numRoot()  )%connect("=>",obj%root(i))
        obj%root2root(obj%numRoot(),i) = 1
        
    else
        print *, "ERROR :: add Node ` soybean >> RootNodeID or StemNodeID should be identified."  
        stop
    endif
    
end subroutine
! ########################################


! ########################################
subroutine addStemSoybean(obj,stemid,rotx,roty,rotz,json)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) :: stemid
    character(*),optional,intent(in) :: json
    real(real64),optional,intent(in) :: rotx,roty,rotz
    integer(int32) :: i

    ! add a stem after stem(stemid)
    do i=1,size(obj%stem)
        if( obj%stem(i)%femdomain%mesh%empty() .eqv. .true. )then
            if(present(json) )then
                call obj%stem(i)%init(json)
                call obj%stem(i)%rotate(x=rotx,y=roty,z=rotz)
                call obj%stem(i)%connect("=>",obj%stem(stemid))
                return
            else
                call obj%stem(i)%init()
                call obj%stem(i)%rotate(x=rotx,y=roty,z=rotz)
                call obj%stem(i)%connect("=>",obj%stem(stemid))
                return
            endif
        else
            cycle
        endif
    enddo



end subroutine
! #############################################################

subroutine deformSoybean(obj,penaltyparameter,groundLevel,disp,x_min,x_max,y_min,y_max,z_min,z_max) 
    class(Soybean_),target,intent(inout) :: obj
    real(real64),optional,intent(in) :: groundLevel,disp(3)
    real(real64),optional,intent(in) :: penaltyparameter,x_min,x_max,y_min,y_max,z_min,z_max
    type(FEMDomainp_),allocatable :: domainsp(:)
    integer(int32),allocatable :: contactList(:,:)
    integer(int32) :: i,j,numDomain,stemDomain,leafDomain,rootDomain
    real(real64) :: penalty,displacement(3),GLevel


    if(.not. allocated(obj%Stem) )then
        print *, "ERROR :: deformSoybean >> no soybean is found!"
        return
    endif
    numDomain = 0
    
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
            endif
        enddo
    endif
    
    allocate(domainsp(numDomain) )
    numDomain=0
    stemDomain=0
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                stemDomain = stemDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%stem(i)%femdomain
            endif
        enddo
    endif

    leafDomain = 0
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                leafDomain = leafDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%leaf(i)%femdomain
            endif
        enddo
    endif

    rootDomain = 0
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                rootDomain = rootDomain + 1
                domainsp(numDomain)%femdomainp =>  obj%root(i)%femdomain
            endif
        enddo
    endif

    contactlist = zeros(numDomain,numDomain)
    if(allocated(obj%stem2stem))then
        do i=1,stemDomain
            do j=1,stemDomain
                contactlist( i, j  ) = obj%stem2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%leaf2stem) )then
        do i=1,leafDomain
            do j=1,stemDomain
                contactlist( i + stemDomain, j  ) = obj%leaf2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%root2stem) )then
        do i=1,rootDomain
            do j=1,stemDomain
                contactlist( i + stemDomain + leafDomain, j  ) = obj%root2stem(i,j)
            enddo
        enddo
    endif

    if(allocated(obj%root2root) )then
        do i=1,rootDomain
            do j=1,rootDomain
                contactlist( i + stemDomain + leafDomain, j+ stemDomain + leafDomain  ) = obj%root2root(i,j)
            enddo
        enddo
    endif
    !call print(contactlist)
    !stop
    call obj%contact%init(femdomainsp=domainsp,contactlist=contactlist)

    ! load material info
    numDomain = 0
    if(allocated(obj%stem) )then
        do i=1,size(obj%stem)
            if(obj%stem(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%stemYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%stemPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%stemDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    if(allocated(obj%leaf) )then
        do i=1,size(obj%leaf)
            if(obj%leaf(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%leafYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%leafPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%leafDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    if(allocated(obj%root) )then
        do i=1,size(obj%root)
            if(obj%root(i)%femdomain%mesh%empty() )then
                cycle
            else
                numDomain = numDomain + 1
                call obj%contact%setYoungModulus(YoungModulus=obj%rootYoungModulus(i),DomainID=numDomain) 
                call obj%contact%setPoissonRatio(PoissonRatio=obj%rootPoissonRatio(i),DomainID=numDomain) 
                call obj%contact%setDensity(density=obj%rootDensity(i),DomainID=numDomain) 
            endif
        enddo
    endif
    !


    penalty = input(default=1000.0d0, option=penaltyparameter)
    
    call obj%contact%setup(penaltyparameter=penalty)

    ! if displacement is set, load displacement
    if(present(disp) )then
        do i=1,numDomain
            call obj%contact%fix(direction="x",disp=disp(1), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
            call obj%contact%fix(direction="y",disp=disp(2), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
            call obj%contact%fix(direction="z",disp=disp(3), DomainID=i,&
                x_min=x_min,x_max=x_max,&
                y_min=y_min,y_max=y_max,&
                z_min=z_min,z_max=z_max)
        enddo    
    endif
    

    Glevel = input(default=0.0d0,option=groundLevel)
    ! under-ground parts are fixed.
    do i=1,numDomain
        call obj%contact%fix(direction="x",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
        call obj%contact%fix(direction="y",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
        call obj%contact%fix(direction="z",disp=0.0d0, DomainID=i,&
            z_max=Glevel)
    enddo

    ! solve > get displacement
    call obj%contact%solver%solve("BiCGSTAB")
    ! update mesh
    call obj%contact%updateMesh()


end subroutine

function getVolumeSoybean(obj,stem,leaf,root) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    ret = ret + obj%stem(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    ret = ret + obj%leaf(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    ret = ret + obj%root(i)%femdomain%getVolume(elem=j)
                enddo
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        ret = ret + obj%stem(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        ret = ret + obj%leaf(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        ret = ret + obj%root(i)%femdomain%getVolume(elem=j)
                    enddo
                endif
            enddo
        endif
    endif

end function

function getBiomassSoybean(obj,stem,leaf,root) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret,volume

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    volume = obj%stem(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume)
                    ret = ret + volume*obj%stem(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) 
                    ret = ret + volume*obj%leaf(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    volume = obj%root(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) 
                    ret = ret + volume*obj%root(i)%drydensity(j) 
                enddo
                
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        volume = obj%stem(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%stem(i)%drydensity(j) 
                    enddo 
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%leaf(i)%drydensity(j) 
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        volume = obj%root(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) 
                        ret = ret + volume*obj%root(i)%drydensity(j) 
                    enddo
                endif
            enddo
        endif
    endif

end function


function getTotalWeightSoybean(obj,stem,leaf,root,waterDensity) result(ret)
    class(Soybean_),intent(in) :: obj
    logical,optional,intent(in) :: stem, leaf, root
    real(real64),optional,intent(in) :: waterDensity
    logical :: all
    integer(int32) :: i,j
    real(real64) :: ret,volume,water_density

    ! kg, m
    water_density=input(default=1000.0d0,option=waterDensity)

    all = .false.
    if(.not.present(stem) .and..not.present(leaf)  )then
        if(.not. present(root) )then
            all = .true.
        endif
    endif

    ret =0.0d0
    if(all)then
        do i=1,size(obj%stem)
            if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                do j=1,obj%stem(i)%femdomain%ne()
                    volume = obj%stem(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%stem(i)%drydensity(j) + volume*obj%stem(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        do i=1,size(obj%leaf)
            if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                do j=1,obj%leaf(i)%femdomain%ne()
                    volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%leaf(i)%drydensity(j) + volume*obj%leaf(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        do i=1,size(obj%root)
            if( .not.obj%root(i)%femdomain%mesh%empty() )then
                do j=1,obj%root(i)%femdomain%ne()
                    volume = obj%root(i)%femdomain%getVolume(elem=j)
                    ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                    ret = ret + volume*obj%root(i)%drydensity(j) + volume*obj%root(i)%watercontent(j)*water_density
                enddo
                
            endif
        enddo
        return
    endif

    if(present(stem))then
        if(stem  .or. all)then
            do i=1,size(obj%stem)
                if( .not.obj%stem(i)%femdomain%mesh%empty() )then
                    do j=1,obj%stem(i)%femdomain%ne()
                        volume = obj%stem(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%stem(i)%drydensity(j) + volume*obj%stem(i)%watercontent(j)*water_density
                    enddo 
                endif
            enddo
        endif
    endif
    if(present(leaf) )then
        if(leaf )then
            do i=1,size(obj%leaf)
                if( .not.obj%leaf(i)%femdomain%mesh%empty() )then
                    do j=1,obj%leaf(i)%femdomain%ne()
                        volume = obj%leaf(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%leaf(i)%drydensity(j) + volume*obj%leaf(i)%watercontent(j)*water_density
                    enddo
                endif
            enddo
        endif
    endif
    if(present(root))then
        if(root)then
            do i=1,size(obj%root)
                if( .not.obj%root(i)%femdomain%mesh%empty() )then
                    do j=1,obj%root(i)%femdomain%ne()
                        volume = obj%root(i)%femdomain%getVolume(elem=j)
                        ! total = total + solid(=drydensity * volume) + fluid (=watercontent * volume)
                        ret = ret + volume*obj%root(i)%drydensity(j) + volume*obj%root(i)%watercontent(j)*water_density
                    enddo
                endif
            enddo
        endif
    endif

end function


!function getBioMassSoybean(obj,stemDensity,leafDensity,rootDensity) result(ret)
!    class(Soybean_),intent(in) :: obj
!    real(real64),optional,intent(in) :: stemDensity,leafDensity,rootDensity
!    logical :: all
!    integer(int32) :: i,j
!    real(real64) :: ret
!
!    ret = 0.0d0
!
!    if(present(stemDensity))then
!        ret = ret + obj%getVolume(stem=.true.) * stemDensity
!    endif
!
!    if(present(leafDensity))then
!        ret = ret + obj%getVolume(leaf=.true.) * leafDensity
!    endif
!
!    if(present(rootDensity))then
!        ret = ret + obj%getVolume(root=.true.) * rootDensity
!    endif
!
!
!
!end function

subroutine removeSoybean(obj)
    class(Soybean_),intent(inout) :: obj

    obj%growth_habit = " "
    obj%growth_stage = " "
    obj%Num_Of_Node = 0
    obj%Num_Of_Root = 0
    
    obj%MaxLeafNum= 300
    obj%MaxRootNum=300
    obj%MaxStemNum= 300

    
    
    obj%ms_node=0
    obj%br_node(:) = 0
    obj%br_from(:) = 0
    obj%ms_length=0.0d0
    obj%br_length(:)=0.0d0
    obj%ms_width=0.0d0
    obj%br_width(:)=0.0d0
    obj%ms_angle_ave=0.0d0
    obj%br_angle_ave(:)=0.0d0
    obj%ms_angle_sig=0.0d0
    obj%br_angle_sig(:)=0.0d0
    

    obj%mr_node=0
    obj%brr_node(:)=0
    obj%brr_from(:)=0
    obj%mr_length=0.0d0
    obj%brr_length(:)=0.0d0
    obj%mr_width=0.0d0
    obj%brr_width(:)=0.0d0
    obj%mr_angle_ave=0.0d0
    obj%brr_angle_ave(:)=0.0d0
    obj%mr_angle_sig=0.0d0
    obj%brr_angle_sig(:)=0.0d0

    obj%peti_size_ave(:)=0.0d0
    obj%peti_size_sig(:)=0.0d0
    obj%peti_width_ave(:)=0.0d0
    obj%peti_width_sig(:)=0.0d0
    obj%peti_angle_ave(:)=0.0d0
    obj%peti_angle_sig(:)=0.0d0

    obj%leaf_angle_ave(:)=0.0d0
    obj%leaf_angle_sig(:)=0.0d0
    obj%leaf_length_ave(:)=0.0d0
    obj%leaf_length_sig(:)=0.0d0
    obj%leaf_width_ave(:)=0.0d0
    obj%leaf_width_sig(:)=0.0d0
    obj%leaf_thickness_ave(:)=0.0d0
    obj%leaf_thickness_sig(:)=0.0d0
    
    obj%Stage="" ! VE, CV, V1,V2, ..., R1, R2, ..., R8
    obj%name=""
    obj%stage_id=0
    obj%dt=0.0d0
    call obj%Seed%remove()
    if (allocated(obj%NodeSystem) ) deallocate(obj%NodeSystem)
    if (allocated(obj%RootSystem) ) deallocate(obj%RootSystem)

    if (allocated(obj%Stem) ) deallocate(obj%Stem)
    if (allocated(obj%Leaf) ) deallocate(obj%Leaf)
    if (allocated(obj%Root) ) deallocate(obj%Root)
    if (allocated(obj%Soil) ) deallocate(obj%Soil)

    ! material info
    if (allocated(obj%stemYoungModulus) ) deallocate(obj%stemYoungModulus)
    if (allocated(obj%leafYoungModulus) ) deallocate(obj%leafYoungModulus)
    if (allocated(obj%rootYoungModulus) ) deallocate(obj%rootYoungModulus)

    if (allocated(obj%stemPoissonRatio) ) deallocate(obj%stemPoissonRatio)
    if (allocated(obj%leafPoissonRatio) ) deallocate(obj%leafPoissonRatio)
    if (allocated(obj%rootPoissonRatio) ) deallocate(obj%rootPoissonRatio)

    if (allocated(obj%stemDensity) ) deallocate(obj%stemDensity)
    if (allocated(obj%leafDensity) ) deallocate(obj%leafDensity)
    if (allocated(obj%rootDensity) ) deallocate(obj%rootDensity)
    

    if(allocated(obj%NodeID_MainStem)) deallocate(obj%NodeID_MainStem)
    if(allocated(obj%NodeID_Branch)) deallocate(obj%NodeID_Branch)
    ! 節-節点データ構造
    call obj%struct%remove(all=.true.)
    if (allocated(obj%leaf2stem) ) deallocate(obj%leaf2stem)
    if (allocated(obj%stem2stem) ) deallocate(obj%stem2stem)
    if (allocated(obj%root2stem) ) deallocate(obj%root2stem)
    if (allocated(obj%root2root) ) deallocate(obj%root2root)
    
    ! 器官オブジェクト配列
    if (allocated(obj%leaf_list) ) deallocate(obj%leaf_list)
    if (allocated(obj%stem_list) ) deallocate(obj%stem_list)
    if (allocated(obj%root_list) ) deallocate(obj%root_list)

    ! シミュレータ
    call obj%contact%remove()
    obj%time=0.0d0
    obj%seed_length=0.0d0
    obj%seed_width=0.0d0
    obj%seed_height=0.0d0
    if (allocated(obj%stem_angle) ) deallocate(obj%stem_angle)
    if (allocated(obj%root_angle) ) deallocate(obj%root_angle)
    if (allocated(obj%leaf_angle) ) deallocate(obj%leaf_angle)

    obj%stemconfig=" "
    obj%rootconfig=" "
    obj%leafconfig=" "

end subroutine

function stemlengthSoybean(obj,StemID) result(ret)
    class(Soybean_),intent(inout) :: obj
    integer(int32),intent(in) :: StemID ! 0, 1, 2...
    integer(int32) :: num_snode,i
    real(real64),allocatable :: ret(:)

    if(StemID==0)then
        ! main stem
        num_snode = size(obj%NodeID_MainStem)
        allocate(ret(num_snode) )
        do i=1,num_snode
            ret(i) = obj%stem(obj%NodeID_MainStem(i))%getLength()
        enddo
    else
        if(StemID >=size(obj%NodeID_Branch)) then
            print *, "ERROR :: stemlengthSoybean >> StemID >=size(obj%NodeID_Branch)"
            ret = zeros(1)
            return
        endif
        ! main stem
        num_snode = size(obj%NodeID_Branch(StemID)%ID)
        allocate(ret(num_snode) )
        do i=1,num_snode
            ret(i) = obj%stem(obj%NodeID_Branch(StemID)%ID(i))%getLength()
        enddo
    endif



end function
! ###################################################################

! ###################################################################
subroutine resizeSoybean(obj,StemID,StemLength)
    class(Soybean_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: StemID
    real(real64),optional,intent(in) :: StemLength(:)
    integer(int32) :: num_snode,i

    if(present(StemID) )then
        if(.not.present(StemLength) )then
            print *, "ERROR :: resizeSoybean >> needs StemLength(:) "
            stop
        endif

        if(StemID==0)then
            ! main stem
            num_snode = size(obj%NodeID_MainStem)
            
            do i=1,num_snode
                call obj%stem(obj%NodeID_MainStem(i))%grow(length=StemLength(i))
            enddo
        else
            if(StemID >=size(obj%NodeID_Branch)) then
                print *, "ERROR :: resizeSoybean >> StemID >=size(obj%NodeID_Branch)"
                
                return
            endif
            ! main stem
            num_snode = size(obj%NodeID_Branch(StemID)%ID)
            
            do i=1,num_snode
                call obj%stem(obj%NodeID_Branch(StemID)%ID(i))%grow(length=StemLength(i) )
            enddo
        endif
        call obj%update()
    

    endif
end subroutine
! ###################################################################


! ###################################################################
function NumberOfBranchSoybean(obj)  result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret = 0
    do i=1,size(obj%br_length)
        if(obj%br_length(i)/=0.0d0 )then
            ret = ret + 1
        endif
    enddo

end function
! ###################################################################

! ###################################################################
function findApicalSoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),allocatable :: ret(:)
    !integer(int32),optional,intent(in) :: StemID
    integer(int32),allocatable :: stem
    integer(int32) :: i,j

    ret = zeros( obj%NumberOfBranch()+1 )

    ret(1)=maxval(obj%NodeID_MainStem(:) )
    do i=1,obj%NumberOfBranch()
        ret(i+1) = maxval(obj%NodeID_Branch(i)%ID(:) )
    enddo
    
!    if(present(StemID) )then
!        if(StemID > size(obj%br_node) )then
!            print *, "ERROR >> findApicalSoybean >> number of branch is ",size(obj%br_node)
!            print *, "StemID=",StemID,"is larger than it."
!            return
!        endif
!
!
!        return
!    endif

end function
! ###################################################################

!function propertiesSoybean(obj) result(ret)
!    class(Soybean_) ,intent(in) :: obj
!
!    
!end function

! ##################################################################
function nnSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of node (point)
    ret = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%stem(i)%femdomain%nn()
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%leaf(i)%femdomain%nn()
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%root(i)%femdomain%nn()
        endif
    enddo

end function
! ##################################################################



! ##################################################################
function neSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of element
    ret = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%stem(i)%femdomain%ne()
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%leaf(i)%femdomain%ne()
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret = ret + obj%root(i)%femdomain%ne()
        endif
    enddo

end function
! ##################################################################


! ##################################################################
function nsSoybean(obj) result(ret)
    class(Soybean_) ,intent(in) :: obj
    integer(int32) :: ret, i

    ! get number of subdomain
    ret = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret = ret + 1
        endif
    enddo

end function


! ##################################################################

function getSubDomainSoybean(obj,id) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: id
    type(FEMDomain_) :: ret
    integer(int32) :: i,ret_id
    
    ! get number of subdomain
    ret_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = obj%stem(i)%femdomain
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",ret_id
    return

    

end function
! ##################################################################

! ##################################################################

subroutine setSubDomainSoybean(obj,domain,id) 
    class(Soybean_),intent(inout) :: obj
    type(FEMDomain_),intent(in) :: domain
    integer(int32),intent(in) :: id
    integer(int32) :: i,domain_id
    
    ! get number of subdomain
    domain_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            domain_id = domain_id + 1
            if(id==domain_id)then
                obj%stem(i)%femdomain = domain
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",domain_id
    return

    

end subroutine
! ##################################################################


! ##################################################################

function getSubDomainTypeSoybean(obj,id) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in) :: id
    character(:),allocatable :: ret
    integer(int32) :: i,ret_id
    
    ! get number of subdomain
    ret_id = 0
    do i=1,size(obj%stem)
        if( .not.obj%stem(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "stem"
                return
            endif
        endif
    enddo
    do i=1,size(obj%leaf)
        if( .not.obj%leaf(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "leaf"
                return
            endif
        endif
    enddo
    do i=1,size(obj%root)
        if( .not.obj%root(i)%femdomain%mesh%empty() ) then
            ret_id = ret_id + 1
            if(id==ret_id)then
                ret = "root"
                return
            endif
        endif
    enddo

    print *, "Caution >> getSubDomainSoybean >> exceed total number of subdomains",ret_id
    return

    

end function
! ##################################################################


! ##################################################################
pure function isMainStemSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    logical :: ret

    ret = exists(vector=obj%NodeID_MainStem, val=StemNodeID)
    
end function
! ##################################################################


! ##################################################################
pure function isBranchStemSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    logical :: ret

    if(obj%branchID(StemNodeID)==0 )then
        ret = .False.
    else
        ret = .True.
    endif

end function
! ##################################################################

pure function branchIDSoybean(obj,StemNodeID) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32),intent(in)  :: StemNodeID
    integer(int32),allocatable :: ret
    integer(int32) :: i,j,k,l,m,n,ret_id

    do i=1, size(obj%NodeID_Branch)
        if(exist(obj%NodeID_Branch(i)%ID(:),StemNodeID ) )then
            ret = i
            return
        endif
    enddo
    ret = 0
    
end function

end module