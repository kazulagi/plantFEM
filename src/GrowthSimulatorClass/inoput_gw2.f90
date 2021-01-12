module inoput_gw3
	use gnuplot
	implicit none
	contains
 !===================================
 subroutine input_gw3(restart,max_angle,LDL,max_length,&
		Apical_dominance_p,seedling_day,harvesting_day,RUE,max_leaf_angle,&
		max_leaf_length,max_leaf_width,leaf_shape_rate,limit_leaf_day,&
		peti_length,max_pod_number,source_convection_rate,source_limitation,&
		seed_pod_rate,threshold)
	real(8),intent(out)::max_angle,max_length,RUE,LDL,max_leaf_angle,&
		max_leaf_length,max_leaf_width,leaf_shape_rate,peti_length,&
		source_convection_rate,source_limitation,seed_pod_rate,threshold
	integer,intent(out)::restart,Apical_dominance_p,limit_leaf_day,max_pod_number
	integer,allocatable,intent(out)::seedling_day(:),harvesting_day(:)
	character*50 infile
	
	print *, "growth simulator 1.0"
	print *, "input file name"
	read(*,*) infile
	
	open(10,file=infile)
	
	allocate(seedling_day(3),harvesting_day(3) )
	
	read(10,*)max_angle,max_leaf_angle
	read(10,*)max_length
	read(10,*)RUE,limit_leaf_day
	read(10,*)seedling_day(1),seedling_day(2),seedling_day(3)
	read(10,*)harvesting_day(1),harvesting_day(2),harvesting_day(3)
	read(10,*)LDL
	read(10,*)Apical_dominance_p
	read(10,*)max_leaf_length,max_leaf_width,peti_length,leaf_shape_rate
	read(10,*)max_pod_number,source_convection_rate,source_limitation,&
		seed_pod_rate
	read(10,*)restart
	
	close(10)
	
 end subroutine input_gw3
 !===================================
 subroutine input_daylength(day,daylength)
	integer,allocatable,intent(out)::day(:,:)
	real(8),allocatable,intent(out)::daylength(:)
	integer i,j,n
	
	open(10,file="daylength.txt")
	read(10,*)n
	allocate(day(n,3),daylength(n) )
	
	do i=1,n
		read(10,*)day(i,1:3),daylength(i)!yy,mm,dd,daylength
	enddo
	close(10)
	
 end subroutine input_daylength
 !===================================
 subroutine gnuplot_gw3(d_connect,d_node,step,day,day_ID)
	integer,intent(in)::d_connect(:,:),step,day(:,:),day_ID
	real(8),allocatable::nod_coord(:,:)
	real(8),intent(in)::d_node(:,:)
	integer,allocatable::elem_nod(:,:)
	integer i,j,n
	
	n=size(d_node,1)
	allocate( nod_coord(n,3),elem_nod(n,2) )
	do i=1,n
		nod_coord(i,1:3)=d_node(i,1:3)
		elem_nod(i,1)=i
		elem_nod(i,2)=d_connect(i,1)
	enddo
	
	call gnuplot_out(elem_nod,nod_coord,step,day,day_ID)
	
	
 end subroutine gnuplot_gw3
 !=================================== 
 subroutine gnuplot_leaf_gw3(leaf_coord,peti_coord,d_node,step)
	real(8), intent(in) ::d_node(:,:),leaf_coord(:,:,:,:),peti_coord(:,:,:)
	integer,intent(in)::step
	integer i,j
	character filename1*17
	character filename2*17
	!======gnuplot動画出力用コマンド===========
	!データ作成用プログラム
	!連番ファイルの作成-----------------------
	i = step
	write (filename1, '("gnu_pet", i6.6, ".txt")') i ! ここでファイル名を生成している
	!write (filename2, '("gnu_pet", i6.6, ".png")') i
	open(40,file=filename1)
	!----------------------------------------
	
	
	!png出力用スクリプトファイル-----------------
	
	!open(50,file='png_script.gp',position='append')
	
	
	!write(50,*)'set output ','"',filename2,'"'
	!write(50,*)'replot ','"',filename1,'" with vectors'
	!=============================================
	
	do i =1, size(peti_coord,1)
		if(d_node(i,5)==0.0d0 )then
			write(40,*)d_node(1,1:3)
			write(40,*)d_node(1,1:3)
			write(40,*) '    '
			write(40,*) '    '
			cycle
		endif
		
		write(40,*)peti_coord(i,1,1:3)
		write(40,*)d_node(i,1:3)
		write(40,*) '    '
		write(40,*) '    '
		write(40,*)peti_coord(i,2,1:3)
		write(40,*)peti_coord(i,1,1:3)
		write(40,*) '    '
		write(40,*) '    '
		write(40,*)peti_coord(i,3,1:3)
		write(40,*)peti_coord(i,1,1:3)
		write(40,*) '    '
		write(40,*) '    '		
		write(40,*)peti_coord(i,4,1:3)
		write(40,*)peti_coord(i,1,1:3)
		write(40,*) '    '
		write(40,*) '    '
	enddo
	
	
	
	close(40)
	!close(50)
	
	i = step
	write (filename1, '("gnu_lef", i6.6, ".txt")') i ! ここでファイル名を生成している
	!write (filename2, '("gnu_pet", i6.6, ".png")') i
	open(40,file=filename1)
	!----------------------------------------
	
	
	!png出力用スクリプトファイル-----------------
	
	!open(50,file='png_script.gp',position='append')
	
	
	!write(50,*)'set output ','"',filename2,'"'
	!write(50,*)'replot ','"',filename1,'" with lines'
	!=============================================
	
	do i =2, size(leaf_coord,1)
		if(d_node(i,5)==0.0d0 )then
			write(40,*)d_node(1,1:3)
			write(40,*)d_node(1,1:3)
			write(40,*) '    '
			write(40,*) '    '
			cycle
		endif		
		
		do j=1, size(leaf_coord,2)
			!plot leaf 
			write(40,*)leaf_coord(i,j,1,1:3)
			write(40,*)leaf_coord(i,j,2,1:3)
			write(40,*)leaf_coord(i,j,3,1:3)
			write(40,*)leaf_coord(i,j,4,1:3)
			write(40,*)leaf_coord(i,j,1,1:3)
			write(40,*) '    '
			write(40,*) '    '
		enddo
	enddo
	
	
	
	close(40)
	!close(50)
	
	
 end subroutine gnuplot_leaf_gw3
 !===================================
 subroutine output_gw3(d_connect,d_node)
	integer,intent(in)::d_connect(:,:)
	real(8),intent(in)::d_node(:,:)
	integer i,n
	
	open(10,file="output_gw.d")
	n=size(d_connect,1)
	
	do i=1,n
		write(10,*)d_connect(i,:)
	enddo
	
	do i=1,n
		write(10,*)d_node(i,:)
	enddo
	
	close(10)
	
 end subroutine output_gw3
 !===================================
 subroutine output_factor(d_node,leaf_coord,occupied_area,step)
	real(8),intent(in)::d_node(:,:),leaf_coord(:,:,:,:)
	real(8),intent(out)::occupied_area
	real(8),allocatable::a(:),b(:)
	real(8) total_factor,r,r_tr,pi,x1,x2,l1,l2,LA
	integer,intent(in)::step
	integer i,j,k
	
	allocate(a(3),b(3))
	
	pi=3.1415926535d0
	
	if(step==1)then
		open(10,file="resource.txt")
		write(10,*)"days, resource/area, LAI, total resource,  area(m^2), leaf area(m^2)"
	else
		open(10,file="resource.txt",position='append')
	endif
	
	r=0.0d0
	!compute occupied areaby circle
	do i=1,size(leaf_coord,1) !node ID
		do j=1,size(leaf_coord,2)! Leaf ID
			do k=1,size(leaf_coord,3)! edge ID
				x1=leaf_coord(i,j,k,1)
				x2=leaf_coord(i,j,k,2)
				r_tr=dsqrt( x1*x1+x2*x2 )
				if(r < r_tr)then
					r=r_tr
				else
					cycle
				endif
			enddo
		enddo
	enddo
	occupied_area=r*r*pi*0.01*0.01
	
	LA=0.0d0
	!compute total leaf area
	do i=1,size(leaf_coord,1) !node ID
		do j=1,size(leaf_coord,2)! Leaf ID
			
			a(:)=leaf_coord(i,j,3,:)-leaf_coord(i,j,1,:)
			b(:)=leaf_coord(i,j,4,:)-leaf_coord(i,j,2,:)
			
			l1=dsqrt(dot_product(a,a) )
			l2=dsqrt(dot_product(b,b) )
			
			LA=LA+0.50d0*l1*l2*0.01*0.01
			
		enddo
	enddo	
	
	
	
	total_factor=0.0d0
	do i=1,size(d_node,1)
		total_factor=total_factor+d_node(i,5)
	enddo
	write(10,*)step,int(total_factor/occupied_area),LA/occupied_area,&
		total_factor,occupied_area,LA
	
	close(10)
	
	
 end subroutine output_factor
 !===================================
 subroutine output_harvest(d_pod,structural_TDW,d_node,seed_pod_rate,&
	occupied_area)
	real(8),intent(in)::d_pod(:,:),structural_TDW,d_node(:,:),&
		seed_pod_rate,occupied_area
	real(8) TDW,Harvest_TDW,seed_weight,yield,soyseed_per_dw,pod_weight
	integer i,j
	
	TDW=structural_TDW
	Harvest_TDW=0.0d0
	do i=1,size(d_pod,1)
		do j=1,size(d_pod,2)
			Harvest_TDW=Harvest_TDW+d_pod(i,j)
		enddo 
	enddo
	
	soyseed_per_dw=2.0d0
	
	
	seed_weight=seed_pod_rate/(1.0d0+soyseed_per_dw*seed_pod_rate)&
		*Harvest_TDW
	pod_weight=	1.0d0/(1.0d0+soyseed_per_dw*seed_pod_rate)&
		*Harvest_TDW
	
	TDW=TDW+seed_weight+pod_weight
	do i=1,size(d_node,1)
		TDW=TDW+d_node(i,5)
	enddo
	
	
	
	open(10,file="harvest.txt")
	write(10,*)"seed weight (g)     :",seed_weight
	write(10,*)"Yield (kg/10a)      :",seed_weight/occupied_area
	write(10,*)"Total Dry Weight(g) :",TDW
	write(10,*)"TDW yield (kg/10a)  :",TDW/occupied_area
	write(10,*)"Harvest Index       :",seed_weight/TDW
	
	close(10)
 
 end subroutine output_harvest
 !===================================
end module inoput_gw3