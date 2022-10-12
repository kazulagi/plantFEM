use MaizeClass
implicit none

type(Maize_) :: maize
character(256) :: fpath

call get_command_argument(number=1,value=fpath)
call maize%init(trim(fpath))
call maize%vtk("maize",single_file=.true.)

end