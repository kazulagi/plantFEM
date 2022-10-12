
! #############################
program main
    use ReactorClass
    use IOClass
    implicit none
  
    real(real64)   :: CO2, O2, C
    integer(int32) :: i
    type(Reactor_) :: reactor
    type(IO_)      :: f(3)
  
    CO2 = 0.0d0 ! mol
    C   = 1.0d0 ! mol
    O2  = 1.0d0 ! mol
  
    call reactor%init()
    call reactor%put("CO2", CO2)
    call reactor%put("C"  ,   C)
    call reactor%put("O2" ,  O2)
  
    ! define reactor with reaction coefficient (in here, = 1.0)
    call reactor%define([str("C"),str("O2")],[str("CO2")],1.0d0)
    ! If
    !  a*[A] + b*[B] -> c*[C]
    ! - d A/dt = k[A]^nA*[B]^nB
    ! then please add args
    !  mol_rate=[a,b,c],reaction_order=[nA,nB])
  
    ! create file
    call f(1)%open("A_mol.txt")
    call f(2)%open("B_mol.txt")
    call f(3)%open("C_mol.txt")
    ! export initial condition
    call f(1)%write(reactor%substances(1)%ptr)
    call f(2)%write(reactor%substances(2)%ptr)
    call f(3)%write(reactor%substances(3)%ptr)
  
    ! run t=0.0 to 10.0s
    do i=1,1000
      call reactor%run(dt=0.010d0)
      call f(1)%write(reactor%substances(1)%ptr)
      call f(2)%write(reactor%substances(2)%ptr)
      call f(3)%write(reactor%substances(3)%ptr)
    enddo  
  
    ! close files
    call f(1)%close()
    call f(2)%close()
    call f(3)%close()
  
  end program main
  ! #############################