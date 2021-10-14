module ControlParameterClass
    use, intrinsic :: iso_fortran_env    
    use std
    implicit none
    type::ControlParameter_
        real(real64)   :: Tol
        integer(int32) :: SimMode
        integer(int32) :: ItrTol
        integer(int32) :: Timestep
    contains
        procedure,public :: init => initControlPara
        procedure,public :: remove => initControlPara
        procedure,public :: SetControlPara => SetControlPara
        procedure,public :: set => SetControlPara
        procedure,public :: export => exportControlPara
        procedure,public :: open => openControlPara
        procedure,public :: save => saveControlPara
    end type ControlParameter_

contains




! ########################################################
subroutine openControlPara(obj,path,name)
    class(ControlParameter_),intent(inout):: obj
    
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","ControlPara",".prop")
        read(f%fh,*) obj%Tol
        read(f%fh,*) obj%SimMode
        read(f%fh,*) obj%ItrTol
        read(f%fh,*) obj%Timestep
        call f%close()
    else
        call f%open(trim(path)//"/ControlPara/","ControlPara",".prop")
        read(f%fh,*) obj%Tol
        read(f%fh,*) obj%SimMode
        read(f%fh,*) obj%ItrTol
        read(f%fh,*) obj%Timestep
        call f%close()
    endif
end subroutine
! ########################################################




! ########################################################
subroutine saveControlPara(obj,path,name)
    class(ControlParameter_),intent(inout):: obj
    
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","ControlPara",".prop")
        write(f%fh,*) obj%Tol
        write(f%fh,*) obj%SimMode
        write(f%fh,*) obj%ItrTol
        write(f%fh,*) obj%Timestep
        call f%close()
    else
        call execute_command_line("mkdir -p "//trim(path)//"/ControlPara")
        call f%open(trim(path)//"/ControlPara/","ControlPara",".prop")
        write(f%fh,*) obj%Tol
        write(f%fh,*) obj%SimMode
        write(f%fh,*) obj%ItrTol
        write(f%fh,*) obj%Timestep
        call f%close()
    endif
end subroutine
! ########################################################





! ########################################################
subroutine initControlPara(obj)
    class(ControlParameter_),intent(inout):: obj

    obj%Tol=1.0e-16
    obj%SimMode=1
    obj%ItrTol=1000
    obj%Timestep=1
end subroutine
! ########################################################

!######### Import Control Parameters #########
subroutine SetControlPara(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(ControlParameter_),intent(out)::obj
    real(real64),optional,intent(in)::OptionalTol
    integer(int32),optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep

    if(present(OptionalSimMode) )then
        obj%SimMode = OptionalSimMode
    else
        obj%SimMode = 1
    endif

    if(present(OptionalTol) )then
        obj%Tol = OptionalTol
    else
        obj%Tol = 1.0e-15
    endif
    
    if(present(OptionalItrTol) )then
        obj%ItrTol = OptionalItrTol
    else
        obj%ItrTol = 100
    endif

    if(present(OptionalTimestep) )then
        obj%Timestep = OptionalTimestep
    else
        obj%Timestep = 100
    endif
    
end subroutine SetControlPara
!######### Import Control Parameters #########


subroutine exportControlPara(obj,restart,path)
    class(ControlParameter_),intent(inout) :: obj
    logical,optional,intent(in) :: restart
    character(*),intent(in) :: path
    type(IO_) :: f

    if(present(restart) )then
        call execute_command_line("mkdir -p "//trim(path)//"/ControlPara")
        call f%open(trim(path)//"/ControlPara/","ControlPara",".res")
        write(f%fh,*) obj%Tol
        write(f%fh,*) obj%SimMode
        write(f%fh,*) obj%ItrTol
        write(f%fh,*) obj%Timestep
        call f%close()
        return
    endif
    
end subroutine

end module ControlParameterClass