use maizeClass
implicit none

type(maize_) :: maize
character(256) :: fpath

call get_command_argument(number=1,value=fpath)
call maize%create(config=trim(adjustl(fpath)))
call maize%vtk(trim(fpath),single_file=.true.)
!call maize%vtk(trim(fpath)+".vtk",single_file=.true.)

end