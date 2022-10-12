
use FEMDomainClass
use SeismicAnalysisClass
implicit none

! modal analysis

type(FEMDomain_) :: object
type(SeismicAnalysis_) :: seismic
type(IO_) :: f
character(256) :: fpath
real(real64) :: ground_level, YoungModulus, PoissonRatio, Density, amp
integer(int32),allocatable :: fix_nodes(:)

call get_command_argument(number=1,value=fpath)

! read mesh file
call object%read(trim(adjustl(fpath) ) )
! read condition file
call f%open(trim(adjustl(fpath)) + ".condition","r")
read(f%fh,*) YoungModulus
read(f%fh,*) PoissonRatio
read(f%fh,*) Density
read(f%fh,*) ground_level
call f%close()

fix_nodes = object%getNodeList(zmax=ground_level)

call seismic%modalAnalysis(&
    femdomain=object,&
    Density = Density*eyes(object%ne() ) ,&
    YoungModulus=YoungModulus*eyes(object%ne() ),&
    PoissonRatio=PoissonRatio*eyes(object%ne() ),&
    fix_node_list_x=fix_nodes,&
    fix_node_list_y=fix_nodes,&
    fix_node_list_z=fix_nodes &
    )

amp = minval([object%x_len(),object%y_len(),object%z_len()])/3.0d0/maxval(seismic%modeVectors)
call seismic%exportModeShape(DomainID=1,&
    femdomain=object,amp=amp,name="mode_1_"//trim(adjustl(fpath) ),MAX_MODE_NUM=3)

! Write frequency
call f%open(trim(adjustl(fpath)) + ".freq","w")
call f%write(seismic%frequency(1:3) )
call f%close()



end