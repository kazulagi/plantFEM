module output_plant_geo
	implicit none
	contains
!================================
 subroutine out_geo(d_connect,d_node,leaf_coord,peti_coord)
	real(8),intent(in)::d_node(:,:),leaf_coord(:,:,:,:),peti_coord(:,:,:)
	real(8),allocatable::x(:),v(:),r
	integer,intent(in)::d_connect(:,:)
	integer i,j,k,itr
	
	allocate(x(3),v(3) )
	open(10,file="plantFEM_sample_out.geo")
	write(10,*)'SetFactory("OpenCASCADE");'
	itr=0
	do i=1,size(d_connect,1)
		itr=itr+1
		x(1)=d_node(i,1)*10.0d0 !cm->mm
		x(2)=d_node(i,2)*10.0d0 !cm->mm
		x(3)=d_node(i,3)*10.0d0 !cm->mm
		
		v(1)=d_node(d_connect(i,1),1)*10.0d0-x(1)
		v(2)=d_node(d_connect(i,1),2)*10.0d0-x(2)
		v(3)=d_node(d_connect(i,1),3)*10.0d0-x(3)
		
		if(dot_product(v,v)==0.0d0)then
			cycle
		endif
		
		write(10,*)'Cylinder(',itr,') = {',&
			x(1),',',&
			x(2),',',&
			x(3),',',&
			v(1),',',&
			v(2),',',&
			v(3),',',&
			' 10.0, 2*Pi}; '
	enddo
	
	do i=1,size(peti_coord,1)
		do j=1,size(peti_coord,2)
			itr=itr+1
			if(j==1)then
				x(1)=d_node(i,1)*10.0d0 !cm->mm
				x(2)=d_node(i,2)*10.0d0 !cm->mm
				x(3)=d_node(i,3)*10.0d0 !cm->mm
			else
				x(1)=peti_coord(i,1,1)*10.0d0 !cm->mm
				x(2)=peti_coord(i,1,2)*10.0d0 !cm->mm
				x(3)=peti_coord(i,1,3)*10.0d0 !cm->mm		
			endif
			
			v(1)=peti_coord(i,j,1)*10.0d0-x(1)
			v(2)=peti_coord(i,j,2)*10.0d0-x(2)
			v(3)=peti_coord(i,j,3)*10.0d0-x(3)
			
			if(dot_product(v,v)==0.0d0)then
				cycle
			endif
			
			write(10,*)'Cylinder(',itr,') = {',&
				x(1),',',&
				x(2),',',&
				x(3),',',&
				v(1),',',&
				v(2),',',&
				v(3),',',&
				' 3.0, 2*Pi}; '
		enddo
	enddo
	
	
 end subroutine out_geo
!================================	
end module output_plant_geo