
! ################################################################################
! followings are exported from mpi_leaflow_1.0.0 @ 2020/06/20
subroutine mpi_greedy_division(infile,my_rank,petot,mpi_elem_nod,mpi_elem_nod_id,elem_num)
	
	integer i,j,k,n,mpi_elem_num,rem,elem_type,current_id
	integer itr,node_ID,l,m,ierr,min_nodeID,start_ID,end_ID
	integer,allocatable::elem_nod(:,:),mpi_elem_eat(:),recv_elem_eat(:),inside(:)
	integer,allocatable,intent(out)::mpi_elem_nod(:,:),mpi_elem_nod_id(:)
	integer,allocatable::iface(:),next_candidate(:)
	integer,intent(in)::my_rank,petot
	integer,intent(out)::elem_num
	character*17,intent(in)::infile
	character*17 outfile
	
	open(10,file=infile,status="old")
	read(10,*)elem_num,elem_type
	
	
	!--- allocate global connectivity --------------
	allocate(elem_nod(elem_num,elem_type),mpi_elem_eat(elem_num),recv_elem_eat(elem_num) )
	allocate(iface(elem_type))
	mpi_elem_eat(:)=0
	recv_elem_eat(:)=0
	call mpi_barrier(mpi_comm_world,ierr)
	!-----------------------------------------------
	
	
	
	!--- input global connectivity -----------------
	do i=1,elem_num
		read(10,*)elem_nod(i,1:elem_type)
	enddo
	!-----------------------------------------------
	close(10)
	
	
	
	!---- compute individual element number --------
	mpi_elem_num=int(elem_num/petot)
	rem=elem_num - mpi_elem_num*petot
	if(my_rank+1 <= rem)then
		mpi_elem_num=mpi_elem_num+1
	endif
	!-----------------------------------------------
	
	
	
	!------- allocate local connectivity -----------
	
	! Global node ID = mpi_nod_coord_id( Local node ID )
	allocate(mpi_elem_nod(mpi_elem_num,elem_type))
	allocate(mpi_elem_nod_id(mpi_elem_num))
	mpi_elem_nod(:,:)=0
	mpi_elem_nod_id(:)=0
	!-----------------------------------------------
	
	
	
	
	!>>>>>>>>>>>>>> Greedy's method >>>>>>>>>>>>>>>>
	
	!----- initialization -----------------
	itr = 0
	current_id=1
	
	!--------------------------------------
	
	
	
	!---- Greedy's method ----------------
	do i=1,petot
	

		if(my_rank+1 == i)then
			outfile="visual_parts.gp"
			if(i==1)then
				!open(20,file=outfile)
				!write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
				!write(20,*) "plot '",outfile,"'"
				!close(20)
			else
				!open(20,file=outfile,position="append")
				!write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
				!write(20,*) "replot '",outfile,"'"
				!close(20)		
			endif
			do j =1,size(mpi_elem_eat)
				if(mpi_elem_eat(j)==0)then
					min_nodeID=j
					exit
				endif
			enddo
			
			mpi_elem_nod(1,:)=elem_nod(min_nodeID,:)
			mpi_elem_nod_id(1)=min_nodeID
			current_id=current_id+1
			mpi_elem_eat(min_nodeID)=1
			start_ID=current_id-1
			end_ID=current_id-1
			
			do 
				
				do j=start_ID,end_ID
					do k=1,elem_type
						node_ID=mpi_elem_nod(j,k)
						
						!-- search and add ---------------
						do l=1,elem_num
							if(mpi_elem_eat(l)==1)then
								cycle
							else
								do m=1,elem_type
									if(node_ID==elem_nod(l,m))then
										mpi_elem_nod(current_id,:)=elem_nod(l,:)
										mpi_elem_nod_id(current_id)=l
										mpi_elem_eat(l)=1
										current_id=current_id+1
										exit
									endif
								enddo
								
							endif
							
							!-- check end ----
							if(current_id==size(mpi_elem_nod,1)+1)then
								exit
							endif			
							!-----------------	
							
							
							
						enddo
						!---------------------------------
						
						
						!-- check end ----
						if(current_id==size(mpi_elem_nod,1)+1)then
							exit
						endif			
						!-----------------	
					enddo
					
					
					!-- check end ----
					if(current_id==size(mpi_elem_nod,1)+1)then
						exit
					endif			
					!-----------------	
				enddo
				
				!-- check end ----
				if(current_id==size(mpi_elem_nod,1)+1)then
					
					print *, "my_rank is",my_rank,"proceeding is ",current_id-1,"/",size(mpi_elem_nod,1)	
					exit
				endif		
				
				!-----------------
				
				
				
				if(end_ID==current_id-1)then
					!-- no harvest ----
					do j =1,size(mpi_elem_eat)
						if(mpi_elem_eat(j)==0)then
							min_nodeID=j
							exit
						endif
					enddo
					
					mpi_elem_nod(current_id,1:elem_type)=elem_nod(j,1:elem_type)
					mpi_elem_nod_id(current_id)=j
					current_id=current_id+1
					mpi_elem_eat(min_nodeID)=1			
				endif	
				start_ID=end_ID+1	
				end_ID=current_id-1
							
				
			enddo
			
			
		endif
		call mpi_barrier(mpi_comm_world,ierr)
		
		!share mpi_elem_eat
		recv_elem_eat(:)=mpi_elem_eat(:)
		call mpi_allreduce(mpi_elem_eat(1),recv_elem_eat(1),elem_num,mpi_integer,mpi_max,mpi_comm_world,ierr)
		mpi_elem_eat(:)=recv_elem_eat(:)
		
	enddo
	
	write(outfile,'("mpi_ele", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	write(10,*)size(mpi_elem_nod,1)
!	do i=1,size(mpi_elem_nod,1)
!		write(10,*)mpi_elem_nod(i,:)
!	enddo
!	close(10)
	
end subroutine mpi_greedy_division

!#######################################################################################

subroutine mpi_node_coord_read(infile_node,mpi_elem_nod,my_rank,petot,mpi_nod_coord,mpi_nod_coord_id,node_num)
	integer,intent(in)::mpi_elem_nod(:,:),my_rank,petot
	character*17,intent(in)::infile_node
	character*17 outfile
	
	integer i,j,k,l,count_nodes,node_ID,new0_or_old1,itr,dim_space,min_nodID
	integer,intent(out)::node_num
	integer,allocatable,intent(out)::mpi_nod_coord_id(:)
	real,allocatable::mpi_nod_coord_id_es(:),null_num(:)
	
	real(8),allocatable,intent(out)::mpi_nod_coord(:,:)
	
	
	!-- count number of nodes in the partition ----
	count_nodes=1
	allocate(mpi_nod_coord_id(size(mpi_elem_nod,1)*size(mpi_elem_nod,2)))
	mpi_nod_coord_id(:)=-1
	do i=1,size(mpi_elem_nod,1)
		do j=1,size(mpi_elem_nod,2)
			if(i*j==1)then
				mpi_nod_coord_id(1)=mpi_elem_nod(i,j)
				count_nodes=count_nodes+1
				cycle
			else
				node_ID=mpi_elem_nod(i,j)
				new0_or_old1=0
				do k=1,count_nodes-1
					if(mpi_nod_coord_id(k)==node_ID)then
						new0_or_old1=1
						exit
					else
						cycle
					endif
				enddo
				
				!-- judge new =0 or old =1--
				if(new0_or_old1==0)then
					mpi_nod_coord_id(count_nodes)=node_ID
					count_nodes=count_nodes+1
				endif
				!---------------------------
			
			endif
		enddo
	enddo
	count_nodes=count_nodes-1
	
	!----------------------------------------------
	
	
	
	
	
	
	
	
	!---- delete remains --------------------------
	allocate(mpi_nod_coord_id_es(count_nodes))
	do i=1,count_nodes
		mpi_nod_coord_id_es(i)=mpi_nod_coord_id(i)
	enddo
	
	deallocate(mpi_nod_coord_id)
	allocate(mpi_nod_coord_id(count_nodes))
	mpi_nod_coord_id(:)=mpi_nod_coord_id_es(:)
	
	!----------------------------------------------
	
	
	
	
	
	!--- sort mpi_nod_coord_id ------------------------
	do i=1,count_nodes
		
		do j=i,count_nodes
			if(mpi_nod_coord_id(j)==minval(mpi_nod_coord_id(i:count_nodes)))then
				min_nodID=mpi_nod_coord_id(j)
				mpi_nod_coord_id(j)=mpi_nod_coord_id(i)
				mpi_nod_coord_id(i)=min_nodID
				
				exit
			endif
		enddo
		
		
	enddo
	!mpi_nod_coord_id(:)=mpi_nod_coord_id_es(:)
	deallocate(mpi_nod_coord_id_es)
	
	!----------------------------------------------
	
	
	
	!---- read coordinates in the partition ----------
	open(10,file=infile_node,status="old")
	read(10,*)node_num,dim_space
	allocate(mpi_nod_coord(count_nodes,dim_space))
	allocate(null_num(dim_space))
	itr=1
	do i=1,node_num
		if(mpi_nod_coord_id(itr)==i)then
			read(10,*)mpi_nod_coord(itr,:)
			itr=itr+1
		else
			read(10,*) null_num(:)
		endif
	enddo
	close(10)
	!-------------------------------------------------
	
	
	!------ output local coordinates -----------------
!	write(outfile,'("mpi_nod", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	do i=1,size(mpi_nod_coord,1)
!		write(10,*)mpi_nod_coord(i,:)
!	enddo
!	close(10)
	!-------------------------------------------------
	
	
	
end subroutine mpi_node_coord_read
!#######################################################################################
subroutine mpi_node_relation(mpi_nod_coord_id,my_rank,petot,node_num,mpi_nod_bound_num,mpi_nod_comm_ID)
	
	integer,intent(in)::mpi_nod_coord_id(:),my_rank,petot,node_num
	integer,allocatable,intent(out)::mpi_nod_bound_num(:),mpi_nod_comm_ID(:,:)
	integer,allocatable::common_flag_loc(:),common_flag_glo(:)
	integer i,j,local_num,global_num,loc_id,ierr,max_comm,itr
	character*17 outfile
	
	allocate(mpi_nod_bound_num(size(mpi_nod_coord_id)))
	
	
	
	!----- detect the number of overlapping for each node -------------------
	do i=1,node_num
		local_num=0
		do j=1,size(mpi_nod_coord_id)
			if(i==mpi_nod_coord_id(j))then
				local_num=1
				loc_id=j
				exit
			endif
		enddo
		global_num=0
		call mpi_allreduce(local_num,global_num,1,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		if(local_num==1)then
			mpi_nod_bound_num(loc_id)=global_num
		endif
	enddo
	mpi_nod_bound_num(:)=mpi_nod_bound_num(:)-1
	!-------------------------------------------------------------------------
	
	
	
	
	!------ max. overlaps ------------------
	max_comm=maxval(mpi_nod_bound_num(:))
	!---------------------------------------
	
	
	
	
	
	!----- get pointer of common nodes to server ID ---------------------------
	allocate(mpi_nod_comm_ID(size(mpi_nod_coord_id),max_comm))
	allocate(common_flag_loc(petot))
	allocate(common_flag_glo(petot))
	mpi_nod_comm_ID(:,:)=0
	
	
	do i=1,node_num
		local_num=0
		do j=1,size(mpi_nod_coord_id)
			if(i==mpi_nod_coord_id(j))then
				local_num=1
				loc_id=j
				exit
			endif
		enddo
		
		global_num=0
		common_flag_glo(:)=0
		common_flag_loc(:)=0
		common_flag_loc(my_rank+1)=local_num
		
		call mpi_allreduce(common_flag_loc(1),common_flag_glo(1),petot,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		
		if(local_num==1)then
			itr=0
			do j=1,petot
				if(common_flag_glo(j)==1 .and. j/=my_rank+1 )then
					itr=itr+1
					mpi_nod_comm_ID(loc_id,itr)=j
				endif
			enddo
		endif
	enddo	
	
	
	!--------------------------------------------------------------------------
	
	
	
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_bou", i6.6, ".txt")') my_rank+1
!	open(10,file=outfile)
!	do i=1,size(mpi_nod_bound_num,1)
!		write(10,*)mpi_nod_bound_num(i),mpi_nod_comm_ID(i,:)
!	enddo
!	close(10)
	!-------------------------------------------------	
	
end subroutine mpi_node_relation
!#######################################################################################



subroutine mpi_read_mat_para(infile_mat,mpi_elem_nod,mpi_elem_nod_id,my_rank,petot,elem_num,mpi_elem_mat,mat_cons)
	
	integer i,j,mpi_elem_num,itr,null_8,mat_num,para_num,exist0_or_not1
	integer,intent(in)::mpi_elem_nod(:,:),mpi_elem_nod_id(:),my_rank,petot,elem_num
	integer,allocatable,intent(out)::mpi_elem_mat(:)
	real(8),allocatable,intent(out)::mat_cons(:,:)
	character*17,intent(in)::infile_mat
	character*17 outfile
	
	mpi_elem_num=size(mpi_elem_nod,1)
	allocate(mpi_elem_mat(mpi_elem_num))
	
	open(10,file=infile_mat,status="old")
	do i=1,elem_num
		exist0_or_not1=1
		do j=1,mpi_elem_num
			if(i==mpi_elem_nod_id(j))then
				read(10,*)mpi_elem_mat(j)
				itr=itr+1
				exist0_or_not1=0
				exit
			endif
		enddo
		if(exist0_or_not1==1)then
			read(10,*)null_8
		endif
	enddo
	
	read(10,*)mat_num,para_num
	allocate(mat_cons(mat_num,para_num))

	do i=1,mat_num
		read(10,*)mat_cons(i,1:para_num)
	enddo
	close(10)
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_mat", i6.6, ".txt")') my_rank+1
!	open(20,file=outfile)
!	do i=1,mpi_elem_num
!		write(20,*)mpi_elem_mat(i),mat_cons( mpi_elem_mat(i) ,:)
!	enddo
!	close(20)
	!-------------------------------------------------			
end subroutine mpi_read_mat_para
!#######################################################################################
subroutine mpi_read_bound_cond(infile_bound,my_rank,mpi_nod_coord_id,&
		mpi_n_bc_nod,mpi_d_bc_nod,mpi_n_bc_val,mpi_d_bc_val)
	
	real(8) read_real
	integer i,j,file_id,n,n_bc_num,d_bc_num,itr,exist0_or_not1,read_int
	integer,intent(in)::my_rank,mpi_nod_coord_id(:)
	character*17,intent(in)::infile_bound
	character*17 outfile
	
	integer,allocatable::mpi_n_bc_nod_es(:),mpi_d_bc_nod_es(:)
	real(8),allocatable::mpi_n_bc_val_es(:),mpi_d_bc_val_es(:)
	
	integer,allocatable,intent(out)::mpi_n_bc_nod(:),mpi_d_bc_nod(:)
	real(8),allocatable,intent(out)::mpi_n_bc_val(:),mpi_d_bc_val(:)
	
	file_id=my_rank+1000
	open(file_id,file=infile_bound,status="old")
	
	
	
	!---- Dirichlet boundary conditions -------------------------
	read(file_id,*) d_bc_num
	allocate(mpi_d_bc_nod_es(d_bc_num),mpi_d_bc_val_es(d_bc_num))
	itr=1
	do i=1,d_bc_num
		read(file_id,*) read_int,read_real
		exist0_or_not1=1
		do j=1,size(mpi_nod_coord_id)
			if(mpi_nod_coord_id(j)==read_int)then
				exist0_or_not1=0
				exit
			endif
		enddo
		if(exist0_or_not1==0)then
			mpi_d_bc_nod_es(itr)=read_int
			mpi_d_bc_val_es(itr)=read_real
			itr=itr+1
		endif
	enddo
	
	if( d_bc_num==1)then
		print *, "Error:mpi_fem_lib.f90 L505 >> no Dirichlet B.C."
		 stop 
	endif
	allocate(mpi_d_bc_nod(itr-1),mpi_d_bc_val(itr-1))
	
	do i=1,itr-1
		mpi_d_bc_nod(i)=mpi_d_bc_nod_es(i)
		mpi_d_bc_val(i)=mpi_d_bc_val_es(i)
		!print *, "d_bc",mpi_d_bc_nod(i),mpi_d_bc_val(i)
	enddo
	
	
	!------------------------------------------------------------
	
	
	
	
	!---- Neumann boundary conditions ----------------------------
	read(file_id,*) n_bc_num

        
	if(n_bc_num==0)then
		close(file_id)
	else
		allocate(mpi_n_bc_nod_es(n_bc_num),mpi_n_bc_val_es(n_bc_num))
		
		itr=1
		
		do i=1,n_bc_num
			read(file_id,*) read_int,read_real
			exist0_or_not1=1
			do j=1,size(mpi_nod_coord_id)
				if(mpi_nod_coord_id(j)==read_int)then
					exist0_or_not1=0
					exit
				endif
			enddo
			if(exist0_or_not1==0)then
				mpi_n_bc_nod_es(itr)=read_int
				mpi_n_bc_val_es(itr)=read_real
				itr=itr+1
			endif
		enddo

		if(itr/=1)then
			allocate(mpi_n_bc_nod(itr-1),mpi_n_bc_val(itr-1))
			do i=1,itr-1
				mpi_n_bc_nod(i)=mpi_n_bc_nod_es(i)
				mpi_n_bc_val(i)=mpi_n_bc_val_es(i)
			enddo
		endif
	endif
	!------------------------------------------------------------	
	close(file_id)
	
	
	!------ output mpi node boundary numbers ---------
!	write(outfile,'("mpi_bcc", i6.6, ".txt")') my_rank+1
!	open(20,file=outfile)
!	write(20,*)"Dirichlet B.C. ::"
!	do i=1,size(mpi_d_bc_nod)
!		write(20,*)mpi_d_bc_nod(i),mpi_d_bc_val(i)
!	enddo
!	write(20,*)"Neumann B.C. ::"
!	do i=1,size(mpi_n_bc_nod)
!		write(20,*)mpi_n_bc_nod(i),mpi_n_bc_val(i)
!	enddo
!	
!	close(20)
	!-------------------------------------------------		
	
	
end subroutine mpi_read_bound_cond
!#######################################################################################
subroutine mpi_read_control_p(infile_control,my_rank,itr_max,tol)
	
	integer,intent(in)::my_rank
	integer,intent(out)::itr_max
	real(8),intent(out)::tol
	character*17,intent(in):: infile_control
	
	open(10,file=infile_control)
	read(10,*)itr_max,tol
	close(10)

end subroutine mpi_read_control_p
!#######################################################################################
