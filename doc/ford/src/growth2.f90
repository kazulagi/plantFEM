program growth3
	use output_plant_geo
	use inoput_gw3
	use growthmod
	implicit none
	
	real(8),allocatable::d_node(:,:),daylength(:),peti_coord(:,:,:),leaf_coord(:,:,:,:),&
		d_pod(:,:)
	real(8) max_angle,max_length,factor,threshold,RUE,LDL,max_leaf_angle,&
		max_leaf_length,max_leaf_width,leaf_shape_rate,peti_length,&
		source_convection_rate,source_limitation,structural_TDW,seed_pod_rate,&
		occupied_area
	integer,allocatable::d_connect(:,:),day(:,:),seedling_day(:),&
		harvesting_day(:)
	integer i,j,num_node,restart,max_br_per_node,start,&
		finish,br_per_node,step,Apical_dominance_p,limit_leaf_day,before,after,&
		max_pod_number,reprocuctive_day
	
	open(100,file="node_number.txt")
	
	!input control RUEmeters
	call input_gw3(restart,max_angle,LDL,max_length,&
		Apical_dominance_p,seedling_day,harvesting_day,RUE,max_leaf_angle,&
		max_leaf_length,max_leaf_width,leaf_shape_rate,limit_leaf_day,peti_length,&
		max_pod_number,source_convection_rate,source_limitation,seed_pod_rate,&
		threshold)
	
	!daylength
	call input_daylength(day,daylength)

	structural_TDW=0.0d0
	reprocuctive_day=0
	
	! initialization
	if(restart==0)then
		!initial analysis
		call initialize_gw3(d_connect,d_node,peti_coord,leaf_coord,max_leaf_angle,&
	max_leaf_length,max_leaf_width,leaf_shape_rate,peti_length,max_length)
	elseif(restart==1)then
		!call restart_gw3()
	else
		stop"invalid restart"
	endif
	
	call detect_start(start,finish,seedling_day,harvesting_day,day)
	
	if(start*finish==0 )stop"day = out of range"
	
	do i=start,finish
		
		call get_old(d_connect,limit_leaf_day)
		
		num_node=size(d_connect,1)
		print *, day(i,1:3),"node number=",num_node
		write(100,*) day(i,1:3),"node number=",num_node
		if(daylength(i)<=LDL )then
			!no extra nodes
			!生殖成長
			reprocuctive_day=reprocuctive_day+1
			
			if(reprocuctive_day==1)then
				allocate(d_pod(size(d_node,1),max_pod_number ) )
				d_pod(:,:)=0.0d0
			else
				if( size(d_pod,1)/=size(d_node,1))then
					stop"invalid seedling_day: second reproductive day came"
				endif
			endif
			
			step=i-start+1
			call update_factor_gw3(d_connect,d_node,RUE,num_node,leaf_coord,day,&
			daylength,limit_leaf_day,i)
			call generate_pod(d_node,d_pod,source_convection_rate,source_limitation)
			call gnuplot_gw3(d_connect,d_node,step,day,i)
			call gnuplot_leaf_gw3(leaf_coord,peti_coord,d_node,step)
			call output_factor(d_node,leaf_coord,occupied_area,step)
			cycle
		endif
		
		
		!栄養成長
		do j=1,num_node
			
			call count_br_per_node(j,br_per_node,d_connect)
			if(br_per_node>=2)then
				cycle
			endif
			
			factor=d_node(j,5)
			if(factor > threshold )then
				before=size(d_connect,1)
				call growth_gw3(j,d_connect,d_node,max_angle,max_length,&
					Apical_dominance_p,max_leaf_angle,&
					max_leaf_length,max_leaf_width,leaf_shape_rate,&
					peti_coord,leaf_coord,peti_length)
				after=size(d_connect,1)
				if(before==after)then
					cycle
				else
					d_node(j,5)=d_node(j,5)-threshold*0.99
					structural_TDW=structural_TDW+threshold*0.99
				endif
				
			else
				cycle
			endif
		enddo
		
		call update_factor_gw3(d_connect,d_node,RUE,num_node,leaf_coord,day,&
			daylength,limit_leaf_day,i)
		step=i-start+1
		call gnuplot_gw3(d_connect,d_node,step,day,i)
		call gnuplot_leaf_gw3(leaf_coord,peti_coord,d_node,step)
		call output_factor(d_node,leaf_coord,occupied_area,step)
	enddo
	call output_gw3(d_connect,d_node)
	call output_harvest(d_pod,structural_TDW,d_node,seed_pod_rate,occupied_area)
	call out_geo(d_connect,d_node,leaf_coord,peti_coord)
	close(100)
end program growth3