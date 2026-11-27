use SoybeanClass
implicit none

type(Soybean_) :: soy

character(:),allocatable :: model_param_file
character(:),allocatable :: genotype_file
character(:),allocatable :: env_file
character(:),allocatable :: logfile_name
character(:),allocatable :: vtk_file_name

model_param_file  = argv() .get. 1  ! Model parameter information
genotype_file     = argv() .get. 2  ! Genetical information
env_file          = argv() .get. 3  ! Environmental information
logfile_name      = argv() .get. 4  ! file name of log-file
!vtk_file_name  = argv() .get. 4  ! file name of vtk-file (optional)

call soy%easy_grow(&
    model_param_file= model_param_file,&
    genotype_file= genotype_file,&
    env_file = env_file, &
    logfile_name=logfile_name &
    )

end