module growthmod
	implicit none
	contains
 !===================================
 subroutine initialize_gw3(d_connect,d_node,peti_coord,leaf_coord,max_leaf_angle,&
	max_leaf_length,max_leaf_width,leaf_shape_rate,peti_length,max_length)
	integer,allocatable,intent(out)::d_connect(:,:)
	real(8),allocatable,intent(out)::d_node(:,:),peti_coord(:,:,:),leaf_coord(:,:,:,:)
	real(8),intent(in)::max_leaf_angle,&
	max_leaf_length,max_leaf_width,leaf_shape_rate,peti_length,max_length
	real(8) r1,r2,x_coord,&
	y_coord,z_coord,direction,pi,angle,x_coord_1,y_coord_1,z_coord_1,&
	peti_RUE,mid_length,width,length
	real(8),allocatable::a(:),b(:),z(:),nvec(:),a_unit(:),b_unit(:)
	
	integer int_num,real_value
	
	integer i,j,n,m,o,parent,itr
	integer :: seedsize
	integer,allocatable :: seed(:)
	real :: rnd
	integer :: c !時間を入れる
	pi=3.14159265350d0
	
	int_num=3
	real_value=5
	allocate(d_connect(2,int_num),d_node(2,real_value))
	allocate(peti_coord(2,4,3),leaf_coord(2,3,4,3)   )
	allocate(a(3),b(3),z(3),nvec(3),a_unit(3),b_unit(3) )
	leaf_coord(:,:,:,:)=0.0d0
	peti_coord(:,:,:)=0.0d0
	!initisalize d_connect
	d_connect(1,1)=1
	d_connect(1,2)=-1 !Apical No. 
	d_connect(1,3)=0
	
	d_connect(2,1)=1
	d_connect(2,2)=0 !Apical No. 
	d_connect(2,3)=0
	
	!initialize d_node
	d_node(1,1)=0.0d0
	d_node(1,2)=0.0d0
	d_node(1,3)=0.0d0
	d_node(1,4)=1.0d0
	d_node(1,5)=0.0d0
	
	d_node(2,1)=0.0d0
	d_node(2,2)=0.0d0
	d_node(2,3)=max_length
	d_node(2,4)=1.0d0
	d_node(2,5)=0.0d0
	
	!initialize peti_coord(:,:,:)============
	
	!random number generation
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
		
	angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle

	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)

	
	length=((rnd-0.50d0)+1.0d0)*peti_length
	
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
	
	direction=rnd*360.0d0
	
	r1=cos(pi*(angle/180.0d0))*length
	r2=sin(pi*(angle/180.0d0))*length
	
	
	x_coord=d_node(2,1)
	y_coord=d_node(2,2)
	z_coord=d_node(2,3)
	
	x_coord=x_coord+r2*cos( pi*(direction/180.0d0) )
	y_coord=y_coord+r2*sin( pi*(direction/180.0d0) )
	z_coord=z_coord+r1
	
	peti_coord(1,:,:)=0.0d0
	
	peti_coord(2,1,1)=x_coord
	peti_coord(2,1,2)=y_coord
	peti_coord(2,1,3)=z_coord
	
	
	!second-fourth
	do i=2,4	
		!random number generation
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
			
		angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==2)then
			peti_RUE=0.1
		else
			peti_RUE=0.05
		endif
		
		length=((rnd-0.50d0)+1.0d0)*max_leaf_length*peti_RUE
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==2)then
			direction=rnd*360.0d0
		elseif(i==3)then
			direction=direction+120
		elseif(i==4)then
			direction=direction-240
		endif
		r1=cos(pi*(angle/180.0d0))*length
		r2=sin(pi*(angle/180.0d0)  )*length
		
		
		x_coord_1=x_coord
		y_coord_1=y_coord
		z_coord_1=z_coord
		
		x_coord_1=x_coord_1+r2*cos( pi*(direction/180.0d0) )
		y_coord_1=y_coord_1+r2*sin( pi*(direction/180.0d0) )
		z_coord_1=z_coord_1+r1
		
		peti_coord(2,i,1)=x_coord_1
		peti_coord(2,i,2)=y_coord_1
		peti_coord(2,i,3)=z_coord_1
	enddo
	
	!leaf generate
	do i=1,3
		!random number generation
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
			
		angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		length=((rnd-0.50d0)+1.0d0)*max_leaf_length
		width=((rnd-0.50d0)+1.0d0)*max_leaf_width
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==1)then
			direction=rnd*360.0d0
		elseif(i==2)then
			direction=direction+120
		elseif(i==3)then
			direction=direction-240
		endif
		
		r1=cos(pi*(angle/180.0d0))*length
		r2=sin(pi*(angle/180.0d0)  )*length
		
		
		x_coord_1=peti_coord(2,i,1)
		y_coord_1=peti_coord(2,i,2)
		z_coord_1=peti_coord(2,i,3)
		
		x_coord_1=x_coord_1+r2*cos( pi*(direction/180.0d0) )
		y_coord_1=y_coord_1+r2*sin( pi*(direction/180.0d0) )
		z_coord_1=z_coord_1+r1
		
		leaf_coord(2,i,3,1)=x_coord_1
		leaf_coord(2,i,3,2)=y_coord_1
		leaf_coord(2,i,3,3)=z_coord_1
		
		leaf_coord(2,i,1,1)=peti_coord(2,i,1)
		leaf_coord(2,i,1,2)=peti_coord(2,i,2)
		leaf_coord(2,i,1,3)=peti_coord(2,i,3)
		!
		a(1)=leaf_coord(2,i,3,1)-peti_coord(2,i,1)
		a(2)=leaf_coord(2,i,3,2)-peti_coord(2,i,2)
		a(3)=leaf_coord(2,i,3,3)-peti_coord(2,i,3)
		
		z(1)=0.0d0
		z(2)=0.0d0
		z(3)=1.0d0
		
		a_unit(:)=a(:)/dsqrt(dot_product(a,a))
		
		b(:)=z(:)-dot_product(a,z)*a_unit(:)
		
		b_unit(:)=b(:)/dsqrt(dot_product(b,b))
		
		nvec(1)=a_unit(2)*b_unit(3)-a_unit(3)*b_unit(2)
		nvec(2)=a_unit(3)*b_unit(1)-a_unit(1)*b_unit(3)
		nvec(3)=a_unit(1)*b_unit(2)-a_unit(2)*b_unit(1)
		
		nvec(:)=nvec(:)/dsqrt(dot_product(nvec,nvec))
		
		mid_length=1.0d0/(1.0d0+leaf_shape_rate)*length
		
		j=2
			
			x_coord_1=peti_coord(2,i,1)
			y_coord_1=peti_coord(2,i,2)
			z_coord_1=peti_coord(2,i,3)
			
			x_coord_1=x_coord_1+mid_length*a_unit(1)
			y_coord_1=y_coord_1+mid_length*a_unit(2)
			z_coord_1=z_coord_1+mid_length*a_unit(3)
			
			leaf_coord(2,i,j,1)=x_coord_1-0.50d0*width*nvec(1)
			leaf_coord(2,i,j,2)=y_coord_1-0.50d0*width*nvec(2)
			leaf_coord(2,i,j,3)=z_coord_1-0.50d0*width*nvec(3)
			
		j=4
			x_coord_1=peti_coord(2,i,1)
			y_coord_1=peti_coord(2,i,2)
			z_coord_1=peti_coord(2,i,3)
			
			x_coord_1=x_coord_1+mid_length*a_unit(1)
			y_coord_1=y_coord_1+mid_length*a_unit(2)
			z_coord_1=z_coord_1+mid_length*a_unit(3)
			
			leaf_coord(2,i,j,1)=x_coord_1+0.50d0*width*nvec(1)
			leaf_coord(2,i,j,2)=y_coord_1+0.50d0*width*nvec(2)
			leaf_coord(2,i,j,3)=z_coord_1+0.50d0*width*nvec(3)
		
		
	enddo
	
 end subroutine initialize_gw3
 !===================================
 subroutine growth_gw3(node,d_connect,d_node,max_angle,max_length,&
	Apical_dominance_p,max_leaf_angle,&
	max_leaf_length,max_leaf_width,leaf_shape_rate,&
	peti_coord,leaf_coord,peti_length)
	
	integer,allocatable::d_connect_es(:,:)
	integer,allocatable,intent(inout)::d_connect(:,:)
	
	real(8),allocatable::d_node_es(:,:),peti_coord_es(:,:,:),&
		leaf_coord_es(:,:,:,:)
	real(8),allocatable,intent(inout)::d_node(:,:),peti_coord(:,:,:),&
		leaf_coord(:,:,:,:)
		
	real(8),intent(in)::max_angle,max_length,peti_length
	real(8),intent(in)::max_leaf_angle,&
	max_leaf_length,max_leaf_width,leaf_shape_rate
	real(8) angle,length,direction,r1,r2,pi,x_coord,y_coord,z_coord
	integer,intent(in)::node,Apical_dominance_p
	
	real(8) x_coord_1,y_coord_1,z_coord_1,peti_RUE,mid_length,width
	real(8),allocatable::a(:),b(:),z(:),nvec(:),a_unit(:),b_unit(:)
	
	integer i,j,n,m,o,parent,itr
	integer :: seedsize
	integer,allocatable :: seed(:)
	real :: rnd
	integer :: c !時間を入れる

	pi=3.14159265350d0
	
	
	allocate(a(3),b(3),z(3),nvec(3),a_unit(3),b_unit(3) )
	
	
	n=size(d_connect,1)
	m=size(d_connect,2)
	o=size(d_node,2)
	allocate(d_connect_es(n+1,m),d_node_es(n+1,o))
	d_connect_es(:,:)=0
	allocate(peti_coord_es(n,4,3),leaf_coord_es(n,3,4,3)   )
	!copy
	do i=1,n
		d_connect_es(i,:)=d_connect(i,:)
		d_node_es(i,:)=d_node(i,:)
	enddo
	
	!generate new node
	d_connect_es(n+1,1)=node
	if(d_connect_es(node,2)==-1  )then
		!basis
		return
	elseif(d_connect_es(node,2)==0  )then
		!aptical node => Get aptical dominance
		d_connect_es(n+1,2)=0
		
		d_connect_es(node,2)=1
		parent=d_connect_es(node,1)
		itr=1
		do
			itr=itr+1
			if( d_connect_es(parent,2)==-1 )then
				!base
				exit
			else
				if(  d_connect_es(parent,2)+1>=itr)then
					d_connect_es(parent,2)=d_connect_es(parent,2)+1
					parent = d_connect_es(parent,1)
				else
					cycle
				endif
			endif
		enddo
		
		
		
	elseif(d_connect_es(node,2)>=Apical_dominance_p  )then
		!generate new aptical
		d_connect_es(n+1,2)=0
		parent=d_connect_es(node,1)
		itr=1
		do
			itr=itr+1
			if( d_connect_es(parent,2)==-1 )then
				!base
				exit
			else
				if(  d_connect_es(parent,2)+1==itr)then
					d_connect_es(parent,2)=d_connect_es(parent,2)+1
					parent = d_connect_es(parent,1)
				else
					exit
				endif
			endif		
		enddo
	else
		!not generate
		return
	endif
	
	d_connect_es(n+1,3)=0
	
	!random number generation
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
		
	angle=((rnd-0.50d0)+1.0d0)*max_angle

	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
	
	length=((rnd-0.50d0)+1.0d0)*max_length
	
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
	
	direction=rnd*360.0d0
	
	r1=abs(cos(pi*(angle/180.0d0)))*length
	r2=abs( sin(pi*(angle/180.0d0)  ))*length
	
	
	x_coord=d_node(node,1)
	y_coord=d_node(node,2)
	z_coord=d_node(node,3)
	
	x_coord=x_coord+r2*cos( pi*(direction/180.0d0) )
	y_coord=y_coord+r2*sin( pi*(direction/180.0d0) )
	z_coord=z_coord+r1
	
	d_node_es(n+1,1)=x_coord
	d_node_es(n+1,2)=y_coord
	d_node_es(n+1,3)=z_coord
	d_node_es(n+1,4)=1.0d0
	d_node_es(n+1,5)=0.0d0
	
	
	deallocate(d_connect,d_node)
	n=size(d_connect_es,1)
	m=size(d_connect_es,2)
	o=size(d_node_es,2)
	allocate(d_connect(n,m),d_node(n,o))
	d_connect(:,:)=d_connect_es(:,:)
	d_node(:,:)=d_node_es(:,:)
	deallocate(d_connect_es,d_node_es)
	
	!====================================
	!generate peti and leaf
	!========================
	
	!expand peti_coord,leaf_coord
	!copy
	n=size(d_connect,1)
	leaf_coord_es(:,:,:,:)=leaf_coord(:,:,:,:)
	peti_coord_es(:,:,:)=peti_coord(:,:,:)
	deallocate(peti_coord,leaf_coord)
	allocate(peti_coord(n,4,3),leaf_coord(n,3,4,3)   )
	
	do i=1,size(peti_coord,1)
		peti_coord(i,:,:)=peti_coord_es(i,:,:)
		leaf_coord(i,:,:,:)=leaf_coord_es(i,:,:,:)
	enddo
	deallocate(peti_coord_es,leaf_coord_es)
	
	
	n=size(d_connect,1)
	!random number generation
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
		
	angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle

	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
	
	length=((rnd-0.50d0)+1.0d0)*peti_length
	
	call system_clock(count=c) !時間を取得
	call random_seed(size=seedsize)
	allocate(seed(seedsize))
	call random_seed(get=seed)
	seed = c !時間を全部に代入
	call random_number(rnd) !rndに乱数をセット
	deallocate(seed)
	
	direction=rnd*360.0d0
	
	r1=cos(pi*(angle/180.0d0))*length
	r2=sin(pi*(angle/180.0d0)  )*length
	
	
	x_coord=d_node(n,1)
	y_coord=d_node(n,2)
	z_coord=d_node(n,3)
	
	x_coord=x_coord+r2*cos( pi*(direction/180.0d0) )
	y_coord=y_coord+r2*sin( pi*(direction/180.0d0) )
	z_coord=z_coord+r1
	
	peti_coord(1,:,:)=0.0d0
	
	peti_coord(n,1,1)=x_coord
	peti_coord(n,1,2)=y_coord
	peti_coord(n,1,3)=z_coord
	
	
	!second-fourth
	do i=2,4	
		!random number generation
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
			
		angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==2)then
			peti_RUE=0.10d0
		else
			peti_RUE=0.050d0
		endif
		
		length=((rnd-0.50d0)+1.0d0)*max_leaf_length*peti_RUE
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==2)then
			direction=rnd*360.0d0
		elseif(i==3)then
			direction=direction+120
		elseif(i==4)then
			direction=direction-240
		endif
		
		r1=abs(cos(pi*(angle/180.0d0)))*length
		r2=abs( sin(pi*(angle/180.0d0)  ))*length
		
		
		x_coord_1=x_coord
		y_coord_1=y_coord
		z_coord_1=z_coord
		
		x_coord_1=x_coord_1+r2*cos( pi*(direction/180.0d0) )
		y_coord_1=y_coord_1+r2*sin( pi*(direction/180.0d0) )
		z_coord_1=z_coord_1+r1
		
		peti_coord(n,i,1)=x_coord_1
		peti_coord(n,i,2)=y_coord_1
		peti_coord(n,i,3)=z_coord_1
	enddo
	
	!leaf generate
	do i=1,3
		!random number generation
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
			
		angle=((rnd-0.50d0)+1.0d0)*max_leaf_angle
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		length=((rnd-0.50d0)+1.0d0)*max_leaf_length
		width=((rnd-0.50d0)+1.0d0)*max_leaf_width
		
		call system_clock(count=c) !時間を取得
		call random_seed(size=seedsize)
		allocate(seed(seedsize))
		call random_seed(get=seed)
		seed = c !時間を全部に代入
		call random_number(rnd) !rndに乱数をセット
		deallocate(seed)
		
		if(i==1)then
			direction=rnd*360.0d0
		elseif(i==2)then
			direction=direction+120
		elseif(i==3)then
			direction=direction-240
		endif
		
		r1=abs(cos(pi*(angle/180.0d0)))*length
		r2=abs( sin(pi*(angle/180.0d0)  ))*length
		
		
		x_coord_1=peti_coord(n,i,1)
		y_coord_1=peti_coord(n,i,2)
		z_coord_1=peti_coord(n,i,3)
		
		x_coord_1=x_coord_1+r2*cos( pi*(direction/180.0d0) )
		y_coord_1=y_coord_1+r2*sin( pi*(direction/180.0d0) )
		z_coord_1=z_coord_1+r1
		
		leaf_coord(n,i,3,1)=x_coord_1
		leaf_coord(n,i,3,2)=y_coord_1
		leaf_coord(n,i,3,3)=z_coord_1
		
		leaf_coord(n,i,1,1)=peti_coord(n,i,1)
		leaf_coord(n,i,1,2)=peti_coord(n,i,2)
		leaf_coord(n,i,1,3)=peti_coord(n,i,3)
		!
		a(1)=leaf_coord(n,i,3,1)-peti_coord(n,i,1)
		a(2)=leaf_coord(n,i,3,2)-peti_coord(n,i,2)
		a(3)=leaf_coord(n,i,3,3)-peti_coord(n,i,3)
		
		z(1)=0.0d0
		z(2)=0.0d0
		z(3)=1.0d0
		
		a_unit(:)=a(:)/dsqrt(dot_product(a,a))
		
		b(:)=z(:)-dot_product(a,z)*a_unit(:)
		
		b_unit(:)=b(:)/dsqrt(dot_product(b,b))
		
		nvec(1)=a_unit(2)*b_unit(3)-a_unit(3)*b_unit(2)
		nvec(2)=a_unit(3)*b_unit(1)-a_unit(1)*b_unit(3)
		nvec(3)=a_unit(1)*b_unit(2)-a_unit(2)*b_unit(1)
		nvec(:)=nvec(:)/dsqrt(dot_product(nvec,nvec))
		mid_length=1.0d0/(1.0d0+leaf_shape_rate)*length
		
		j=2
			
			x_coord_1=peti_coord(n,i,1)
			y_coord_1=peti_coord(n,i,2)
			z_coord_1=peti_coord(n,i,3)
			
			x_coord_1=x_coord_1+mid_length*a_unit(1)
			y_coord_1=y_coord_1+mid_length*a_unit(2)
			z_coord_1=z_coord_1+mid_length*a_unit(3)
			
			leaf_coord(n,i,j,1)=x_coord_1-0.50d0*width*nvec(1)
			leaf_coord(n,i,j,2)=y_coord_1-0.50d0*width*nvec(2)
			leaf_coord(n,i,j,3)=z_coord_1-0.50d0*width*nvec(3)
			
		j=4
			x_coord_1=peti_coord(n,i,1)
			y_coord_1=peti_coord(n,i,2)
			z_coord_1=peti_coord(n,i,3)
			
			x_coord_1=x_coord_1+mid_length*a_unit(1)
			y_coord_1=y_coord_1+mid_length*a_unit(2)
			z_coord_1=z_coord_1+mid_length*a_unit(3)
			
			leaf_coord(n,i,j,1)=x_coord_1+0.50d0*width*nvec(1)
			leaf_coord(n,i,j,2)=y_coord_1+0.50d0*width*nvec(2)
			leaf_coord(n,i,j,3)=z_coord_1+0.50d0*width*nvec(3)
		
		
	enddo

 end subroutine growth_gw3
 !===================================
 subroutine update_factor_gw3(d_connect,d_node,RUE,num_node,leaf_coord&
	,day,daylength,limit_leaf_day,day_ID)
	real(8),intent(inout)::d_node(:,:)
	real(8),intent(in)::RUE,leaf_coord(:,:,:,:),daylength(:)
	real(8),allocatable::photosynthesis(:,:),radiation(:,:)
	real(8) factor,light_area,total_area,old_RUEmeter
	integer,intent(in)::d_connect(:,:),num_node,day_ID,day(:,:),limit_leaf_day
	integer i,j,month,node_ID,leaf_ID
	
	allocate(photosynthesis(num_node,3),radiation(12,2) )
	photosynthesis(:,:)=0.0d0
	open(60,file="radiation.txt")
	do i=1,12
		read(60,*)radiation(i,1:2)
	enddo
	close(60)
	
	!生成してから1日経ってから光合成開始
	!日照面積判定
	do i=1,num_node
		do j=1,size(leaf_coord,2)
			!compute light area
			node_ID=i
			leaf_ID=j
			call detect_light_area(leaf_coord,light_area,total_area,node_ID,leaf_ID)
		
			photosynthesis(i,1)=light_area
			photosynthesis(i,2)=total_area
			!光合成量＝日射時間(hr)×面積当たり時間当たり日射量(MJ/m^2/hr)×日照面積(m^2)×日射量あたり光合成(g/MJ)
			month=day(day_ID,2)
			if( radiation(month,1)==0.0d0 )then
				photosynthesis(i,3)=0.0d0
			endif
			
			if(d_connect(i,3)==-1 )then
				old_RUEmeter=0.0d0
			else
				old_RUEmeter=dble(d_connect(i,3))/dble(limit_leaf_day)
				if(old_RUEmeter<0)stop"debugb"
			endif
			
			photosynthesis(i,3)=daylength(day_ID)*radiation(month,2)/radiation(month,1)&
				*light_area*RUE*old_RUEmeter
		enddo
	enddo
	
	
	do i=1,num_node
		!compute factor
		factor=d_node(i,5)
		
		factor=factor+photosynthesis(i,3)
		
		d_node(i,5)=factor
	enddo
	
	
 end subroutine update_factor_gw3
 !===================================
 subroutine detect_start(start,finish,seedling_day,harvesting_day,day)
	integer,intent(out)::start,finish
	integer,intent(in)::seedling_day(:),harvesting_day(:),day(:,:)
	integer i,j,n,yy,mm,dd,diff
	
	do i=1,size(day,1)
		yy=day(i,1)
		mm=day(i,2)
		dd=day(i,3)
		
		diff=abs( seedling_day(1)-yy )&
			+abs( seedling_day(2)-mm )&
			+abs( seedling_day(3)-dd )
		
		if(diff==0 )then
			start=i
			exit
		endif
		
	enddo
	
	do i=1,size(day,1)
		yy=day(i,1)
		mm=day(i,2)
		dd=day(i,3)
		
		diff=abs( harvesting_day(1)-yy )&
			+abs( harvesting_day(2)-mm )&
			+abs( harvesting_day(3)-dd )
		
		if(diff==0 )then
			finish=i
			exit
		endif
		
	enddo
	
	
 end subroutine detect_start
 !===================================
  subroutine count_br_per_node(node_number,br_per_node,d_connect)
	integer,intent(in)::node_number,d_connect(:,:)
	integer,intent(out)::br_per_node
	integer i
	
	br_per_node=0
	do i=1,size(d_connect,1)
		if(node_number==d_connect(i,1) )then
			br_per_node=br_per_node+1
		else
			cycle
		endif
	enddo
	if(br_per_node>=3)stop"debug"
	
 end subroutine count_br_per_node
 !===================================
 subroutine detect_light_area(leaf_coord,light_area,total_area,node_ID,leaf_ID)
	real(8),intent(in)::leaf_coord(:,:,:,:)
	real(8),intent(out)::light_area,total_area
	real(8),allocatable::mid_x(:),shape_function(:),evaluate_point(:,:),eva_x(:),rvec(:)&
		,x1(:),x2(:),y1(:),y2(:)
	real(8) gzi_1,gzi_2,eva_r,mid_r,both_r,l1,l2,l3
	integer,intent(in)::node_ID,leaf_ID
	integer i,j,k,l,m,shade_or_not,count_shade
	
	allocate(mid_x(3),shape_function(4),evaluate_point(4,2),eva_x(3),rvec(2),x1(2),x2(2))
	allocate(y1(3),y2(3))
	
	!2D linear interpolation,4 points
	evaluate_point(1,1)=-0.50d0
	evaluate_point(1,2)=-0.50d0
	evaluate_point(2,1)=-0.50d0
	evaluate_point(2,2)=0.50d0
	evaluate_point(3,1)=0.50d0
	evaluate_point(3,2)=0.50d0
	evaluate_point(4,1)=0.50d0
	evaluate_point(4,2)=-0.50d0
	
	y1(1:3)=leaf_coord(node_ID,leaf_ID,1,1:3)-leaf_coord(node_ID,leaf_ID,3,1:3)
	y2(1:3)=leaf_coord(node_ID,leaf_ID,2,1:3)-leaf_coord(node_ID,leaf_ID,4,1:3)
	l1=dsqrt( dot_product(y1,y1) )
	l2=dsqrt( dot_product(y2,y2) )
	total_area=0.50d0*l1*l2/100.0d0/100.0d0
	
	count_shade=0
	do k=1,4 !evaluate points
		gzi_1=evaluate_point(k,1)
		gzi_2=evaluate_point(k,2)
		shape_function(1)=0.250d0*(1.0d0-gzi_1)*(1.0d0-gzi_1)
		shape_function(2)=0.250d0*(1.0d0-gzi_1)*(1.0d0+gzi_1)
		shape_function(3)=0.250d0*(1.0d0+gzi_1)*(1.0d0+gzi_1)
		shape_function(4)=0.250d0*(1.0d0+gzi_1)*(1.0d0-gzi_1)
		
		eva_x(:)=0.0d0
		do l=1,4 
			eva_x(:)=eva_x(:)+leaf_coord(node_ID,leaf_ID,l,:)&
				*shape_function(l)
		enddo
		rvec(:)=leaf_coord(node_ID,leaf_ID,k,1:2)
		eva_r=dsqrt( dot_product(rvec-eva_x(1:2) ,rvec-eva_x(1:2)  ) )
		
		shade_or_not=0
		
		do i=1,size(leaf_coord,1)
			do j=1,size(leaf_coord,2)
				mid_x(:)=0.0d0
				do l=1,4
					mid_x(:)=mid_x(:)+0.250d0*leaf_coord(i,j,l,:)
				enddo
				
				!if z coordinate of the midpoint of the leaf > evaluate point
				! => possible node
				if(mid_x(3) < eva_x(3) )then
					cycle
				endif
				
				x1(1:2)=leaf_coord(i,j,1,1:2)-leaf_coord(i,j,3,1:2)
				x2(1:2)=leaf_coord(i,j,2,1:2)-leaf_coord(i,j,4,1:2)
				
				mid_r=dsqrt( dot_product(x1,x1) )+dsqrt( dot_product(x2,x2) )
				mid_r=0.50d0*mid_r
				
				both_r=dsqrt( dot_product(mid_x(1:2)-eva_x(1:2),mid_x(1:2)-eva_x(1:2)))
				
				if(both_r > eva_r+mid_r)then
					cycle
				else
					shade_or_not=1
					exit
				endif		
			enddo
			
			if(shade_or_not==1)then
				exit
			endif
		enddo
		
		if(shade_or_not==1)then
			cycle
		else
			count_shade=count_shade+1
		endif	
	enddo
	
	light_area=total_area/4.0d0*dble(count_shade)
	
 end subroutine detect_light_area
 !===================================
 subroutine get_old(d_connect,limit_leaf_day)
	integer,intent(inout)::d_connect(:,:)
	integer,intent(in)::limit_leaf_day
	integer i
	
	do i=1,size(d_connect,1)
		if(d_connect(i,3)==0 )then
			!first day 
			d_connect(i,3)=limit_leaf_day
		elseif(d_connect(i,3)==1 )then
			d_connect(i,3)=-1
		elseif(d_connect(i,3)==-1 )then
			d_connect(i,3)=-1
		elseif(limit_leaf_day>=d_connect(i,3) .and. d_connect(i,3)>=2)then
			d_connect(i,3)=d_connect(i,3)-1
		else
			stop"invalid d_connect,3"
		endif
	enddo
	
 end subroutine get_old
 !===================================
 subroutine generate_pod(d_node,d_pod,source_convection_rate,source_limitation)
	real(8),intent(inout)::d_node(:,:),d_pod(:,:)
	real(8),intent(in)::source_convection_rate,source_limitation
	real(8) total_source,extra_source
	integer i,j,pn
	
	do i=1,size(d_node,1)
		total_source=d_node(i,5)
		pn=0
		do j=1,size(d_pod,2)
			if(d_pod(i,j)==source_limitation)then
				cycle
			elseif(d_pod(i,j)<source_limitation )then
				pn=j
				exit
			else
				stop"invalid source/pod"
			endif
		enddo
		
		if(pn==0)then
			!sink was occupied
			cycle
		endif
		
		if(total_source>=source_convection_rate)then
			d_node(i,5)=total_source-source_convection_rate
			d_pod(i,pn)=d_pod(i,pn)+source_convection_rate
			
			
		elseif(source_convection_rate>total_source .and. &
			total_source>0.0d0)then
			d_pod(i,pn)=d_pod(i,pn)+total_source
			d_node(i,5)=0.0d0
		elseif(total_source==0.0d0)then
			cycle
		else
			stop"invalid source < 0"
		endif
		
		extra_source=0.0d0
		do j=1,size(d_pod,2)
			if(extra_source/=0.0d0)then
				d_pod(i,j)=d_pod(i,j)+extra_source
			endif
			
			if(d_pod(i,j)>source_limitation)then
				extra_source=d_pod(i,j)-source_limitation
				d_pod(i,j)=source_limitation
			endif
		enddo
		
		if(extra_source/=0.0d0)then
			d_node(i,5)=d_node(i,5)+extra_source
			extra_source=0.0d0
		endif
		
	enddo
	
	
	
 end subroutine generate_pod
 !===================================
end module growthmod