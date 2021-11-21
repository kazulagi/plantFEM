program main
    use plantFEM
    implicit none
  
    real(real64)   :: CO2, O2, C
    real(real64)   :: dt,t
    integer(int32) :: i
    type(Reactor_) :: reactor
    type(Logger_)  :: Logger
    type(Console_) :: console
  
    CO2 = 0.0d0 ! mol
    C   = 1.0d0 ! mol
    O2  = 1.0d0 ! mol
  
    ! initalize reactor
    call reactor%init()
    call reactor%put("CO2", CO2)
    call reactor%put("C"  ,   C)
    call reactor%put("O2" ,  O2)
    ! define reactor with reaction coefficient (in here, = 1.0)
    call reactor%define([str("C"),str("O2")],[str("CO2")],0.10d0)
    ! If (a/=1 or b/=1 or c/=1) and  (nA/=1 or nB/=1 or nC/=1) and
    !  a*[A] + b*[B] -> c*[C]
    ! - d A/dt = k[A]^nA*[B]^nB
    ! then please add args
    !  mol_rate=[a,b,c],reaction_order=[nA,nB])
  
    ! initialize data-logger
    call logger%set("CO2",CO2)
    call logger%set("C",C  )
    call logger%set("O2", O2)
    call logger%start()
    
    t = 0.0d0
    do
      ! read values from console
      call console%log("Type [exit] to exit/if not, type [continue]>>>")
      call console%read()
      
      if(console%asChar()=="exit") exit

      call console%log("How much C (mol/L) do you add?>>>")
      call console%read() 
      C = C + console%asReal()
      call console%log("How much O2 (mol/L) do you add?>>>")
      call console%read() 
      O2 = O2 + console%asReal()
      call console%log("How much CO2 (mol/L) do you add?>>>")
      call console%read() 
      CO2 = CO2 + console%asReal()
      

      dt = 0.010d0
      ! run t=0.0 to 10.0s
      do i=1,1000
        t = t+dt
        call reactor%run(dt=dt)
        ! save data 
        call logger%save(t = t)
      enddo

    enddo

  end program main