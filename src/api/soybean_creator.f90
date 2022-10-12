use SoybeanClass
implicit none

type(Soybean_) :: soybean
character(256) :: fpath

call get_command_argument(number=1,value=fpath)
call soybean%create(config=trim(adjustl(fpath)))
call soybean%vtk(trim(fpath),single_file=.true.)

end