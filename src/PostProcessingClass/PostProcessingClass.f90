module PostProcessingClass
    use, intrinsic :: iso_fortran_env
	use FEMDomainClass

	implicit none

	type :: PostProcessing_
		character(:),allocatable :: filename

		character(:),allocatable :: filehead
		integer(int32) :: file_zfill_len
		integer(int32) :: number_of_file
		character(:),allocatable :: filetail
		integer(int32) :: start_idx
		integer(int32) :: end_idx
		character(:),allocatable :: file_info

		type(FEMDomain_) :: buf
		
		logical :: initialized = .false.
	contains
		procedure ::  femdomain => femdomainPostProcessing
		procedure ::  scalar => scalarPostProcessing
	end type

	interface to_PostProcessing
		module procedure to_PostProcessingPostProcessingClass
	end interface

contains

! ############################################################################
function to_PostProcessingPostProcessingClass(filename) result(ret)
	character(*),intent(in) :: filename
	type(PostProcessing_) :: ret
	integer(int32) :: percent_idx, d_idx,i,j
	character(:),allocatable :: command
	type(IO_) :: f

	! sentence analysis
	if( ("%" .in. filename) .and. ("d" .in. filename)   )then
		percent_idx = index(filename,"%")
		d_idx       = index(filename( percent_idx+1: ),"d")+percent_idx
		ret%filehead = filename(1:percent_idx-1 )
		ret%filetail = filename(d_idx+1: )
		ret%file_zfill_len = fint( filename(percent_idx+1:d_idx-1) )
		
		! search number of file
		command = "ls "+ret%filehead+"*"+ret%filetail+"| wc -l > .postprocessing_buf.txt"
		call system(command)
		call f%open(".postprocessing_buf.txt","r")
		ret%number_of_file = fint(f%readline() )
		call f%close()

		if(ret%number_of_file==0) return

		i = -1
		ret%start_idx = -1
		ret%end_idx = -1
		do 
			i = i + 1
			if(ret%start_idx==-1)then
				if(f%exists( ret%filehead + zfill(i,ret%file_zfill_len)+ret%filetail ) )then
					ret%start_idx = i
					cycle
				endif
			else
				do j=1,ret%number_of_file
					if(.not.f%exists( ret%filehead + zfill(i+j,ret%file_zfill_len)+ret%filetail ) )then
						ret%end_idx = i+j-1
						exit
					endif
				enddo
				exit
			endif
		enddo

		ret%file_info = "["&
			+ret%filehead+zfill(ret%start_idx,ret%file_zfill_len)+ret%filetail &
			+",...," &
			+ret%filehead+zfill(ret%end_idx  ,ret%file_zfill_len)+ret%filetail &
			+"]"
		command = "rm .postprocessing_buf.txt"
		call system(command)
	else
		ret%filehead=trim(filename)
		ret%filetail=""
		ret%file_zfill_len=0
		if(f%exists(trim(filename)) )then
			ret%number_of_file=1
		else
			ret%number_of_file=0
		endif
		ret%file_info = "["+ret%filehead+"]"
	endif

	ret%initialized = .true.
end function
! ############################################################################



! ############################################################################
subroutine scalarPostProcessing(this,scalar,fileIdx)
	class(PostProcessing_),intent(inout) :: this
	real(real64),allocatable,intent(inout) :: scalar(:)
	integer(int32),optional,intent(in) :: fileIdx
	type(FEMDomain_) :: femdomain
	character(:),allocatable :: filename

	if(this%initialized)then
		if(this%number_of_file==0)then
			print *, "[ERROR] file is not initialized!"
			print *, "file name is "+this%filehead
			return
		endif

		if(this%file_zfill_len==0 )then
			! single file
			if(present(fileIdx) )then
				print *, "[Caution] single file mode >> "
				print *, "fileIdx is not necessary."
			endif
			call this%buf%read_SCALAR(this%filehead)
			print *, "imported ",this%filehead
			scalar = this%buf%PhysicalField(1)%scalar
		else
			if(.not.present(fileIdx) )then
				print *, "[ERROR] there are multiple files. please input fileIdx"
				return
			endif
			filename = this%filehead + zfill(fileIdx,this%file_zfill_len)+this%filetail
			
			call this%buf%read_SCALAR(trim(filename))

			print *, "imported ",trim(filename)
			scalar = this%buf%PhysicalField(1)%scalar
		endif
	else
		print *, "[ERROR] PostProcesser is not initialized!"
		print *, "Please call"
		print *, "ret = to_PostProcessing(filename)"
	endif

end subroutine
! ############################################################################



! ############################################################################
subroutine femdomainPostProcessing(this,femdomain,fileIdx)
	class(PostProcessing_),intent(inout) :: this
	integer(int32),optional,intent(in) :: fileIdx
	type(FEMDomain_),intent(inout) :: femdomain
	character(:),allocatable :: filename

	if(this%initialized)then
		if(this%number_of_file==0)then
			print *, "[ERROR] file is not initialized!"
			print *, "file name is "+this%filehead
			return
		endif

		if(this%file_zfill_len==0 )then
			! single file
			if(present(fileIdx) )then
				print *, "[Caution] single file mode >> "
				print *, "fileIdx is not necessary."
			endif
			call femdomain%read(this%filehead)
			print *, "imported ",this%filehead
		else
			if(.not.present(fileIdx) )then
				print *, "[ERROR] there are multiple files. please input fileIdx"
				return
			endif
			filename = this%filehead + zfill(fileIdx,this%file_zfill_len)+this%filetail
			call femdomain%read(trim(filename))
			print *, "imported ",trim(filename)
		endif
	else
		print *, "[ERROR] PostProcesser is not initialized!"
		print *, "Please call"
		print *, "ret = to_PostProcessing(filename)"
	endif

end subroutine
! ############################################################################


end module