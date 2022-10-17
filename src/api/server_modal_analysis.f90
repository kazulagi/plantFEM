
use FEMDomainClass
use SeismicAnalysisClass
implicit none

! modal analysis

type(FEMDomain_) :: object
type(SeismicAnalysis_) :: seismic
type(IO_) :: f
character(256) :: fpath
real(real64) :: ground_level, YoungModulus, PoissonRatio, Density, amp,&
    fix_boundary_xmin,fix_boundary_xmax,fix_boundary_ymin,fix_boundary_ymax
integer(int32),allocatable :: fix_nodes_x(:),fix_nodes_y(:),fix_nodes_z(:)

call get_command_argument(number=1,value=fpath)

! read mesh file
call object%read(trim(adjustl(fpath) ) )
! read condition file
call f%open(trim(adjustl(fpath)) + ".condition","r")
read(f%fh,*) YoungModulus
read(f%fh,*) PoissonRatio
read(f%fh,*) Density
read(f%fh,*) ground_level
read(f%fh,*) fix_boundary_xmin
read(f%fh,*) fix_boundary_xmax
read(f%fh,*) fix_boundary_ymin
read(f%fh,*) fix_boundary_ymax
call f%close()

fix_nodes_x = object%getNodeList(zmax=ground_level)
fix_nodes_y = object%getNodeList(zmax=ground_level)
fix_nodes_z = object%getNodeList(zmax=ground_level)

fix_nodes_x = fix_nodes_x // object%getNodeList(xmax=fix_boundary_xmin)
fix_nodes_y = fix_nodes_y // object%getNodeList(xmax=fix_boundary_xmin)
fix_nodes_z = fix_nodes_z // object%getNodeList(xmax=fix_boundary_xmin)

fix_nodes_x = fix_nodes_x // object%getNodeList(xmin=fix_boundary_xmax)
fix_nodes_y = fix_nodes_y // object%getNodeList(xmin=fix_boundary_xmax)
fix_nodes_z = fix_nodes_z // object%getNodeList(xmin=fix_boundary_xmax)

fix_nodes_x = fix_nodes_x // object%getNodeList(ymax=fix_boundary_ymin)
fix_nodes_y = fix_nodes_y // object%getNodeList(ymax=fix_boundary_ymin)
fix_nodes_z = fix_nodes_z // object%getNodeList(ymax=fix_boundary_ymin)

fix_nodes_x = fix_nodes_x // object%getNodeList(ymin=fix_boundary_ymax)
fix_nodes_y = fix_nodes_y // object%getNodeList(ymin=fix_boundary_ymax)
fix_nodes_z = fix_nodes_z // object%getNodeList(ymin=fix_boundary_ymax)

call seismic%modalAnalysis(&
    femdomain=object,&
    Density = Density*eyes(object%ne() ) ,&
    YoungModulus=YoungModulus*eyes(object%ne() ),&
    PoissonRatio=PoissonRatio*eyes(object%ne() ),&
    fix_node_list_x=fix_nodes_x,&
    fix_node_list_y=fix_nodes_y,&
    fix_node_list_z=fix_nodes_z &
    )

amp = minval([object%x_len(),object%y_len(),object%z_len()])/3.0d0/maxval(seismic%modeVectors)
call seismic%exportModeShape(DomainID=1,&
    femdomain=object,amp=amp,name="mode_1_"//trim(adjustl(fpath) ),MAX_MODE_NUM=3)

! Write frequency
call f%open(trim(adjustl(fpath)) + ".freq","w")
call f%write(seismic%frequency(1:3) )
call f%close()



end