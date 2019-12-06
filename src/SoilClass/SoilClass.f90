module SoilClass
    use, intrinsic :: iso_fortran_env
    use SiCroF
    implicit none

    type :: Soil_
        type(FEMDomain_) :: FEMDomain

        real(real64) :: depth
        real(real64) :: length
        real(real64) :: width
        real(real64) :: x,y,z ! center coordinate

        ! ================
        ! Nutorient
        !------------
        real(real64) :: N_kg
        real(real64) :: P_kg
        real(real64) :: K_kg
        real(real64) :: Ca_kg
        real(real64) :: Mg_kg
        real(real64) :: S_kg
        !------------
        real(real64) :: Fe_kg
        real(real64) :: Mn_kg
        real(real64) :: B_kg
        real(real64) :: Zn_kg
        real(real64) :: Mo_kg
        real(real64) :: Cu_kg
        real(real64) :: Cl_kg
        ! ================

        
        ! ================
        ! Soil phyisical parameters
        real(real64) :: C_N_ratio
        real(real64) :: EC
        ! ================


    contains
        procedure :: init => initSoil
        procedure :: fertilize => fertilizeSoil
        procedure :: diagnosis => diagnosisSoil
        procedure :: export => exportSoil
    end type

contains

! ################################################################
subroutine initSoil(obj,depth,length,width,x,y,z)
    class(Soil_),intent(inout)::obj
    real(real64),optional,intent(in):: depth,length,width,x,y,z ! cm

    obj%depth = input(default=-1.0d0,option=depth )
    obj%length= input(default=1.0d0,option=length)
    obj%width = input(default=1.0d0,option=width )
    obj%x     = input(default=0.0d0,option=x )
    obj%y     = input(default=0.0d0,option=y)
    obj%z     = input(default=0.0d0,option=z )

end subroutine
! ################################################################


! ################################################################
subroutine fertilizeSoil(obj,N_kg,P_kg,K_kg,Ca_kg,Mg_kg,S_kg,Fe_kg,&
    Mn_kg,B_kg,Zn_kg,Mo_kg,Cu_kg,Cl_kg)
    
    class(Soil_),intent(inout)::obj
    ! ================
    real(real64),optional,intent(in) :: N_kg
    real(real64),optional,intent(in) :: P_kg
    real(real64),optional,intent(in) :: K_kg
    real(real64),optional,intent(in) :: Ca_kg
    real(real64),optional,intent(in) :: Mg_kg
    real(real64),optional,intent(in) :: S_kg
    ! ================
    real(real64),optional,intent(in) :: Fe_kg
    real(real64),optional,intent(in) :: Mn_kg
    real(real64),optional,intent(in) :: B_kg
    real(real64),optional,intent(in) :: Zn_kg
    real(real64),optional,intent(in) :: Mo_kg
    real(real64),optional,intent(in) :: Cu_kg
    real(real64),optional,intent(in) :: Cl_kg
    ! ================

    obj%N_kg    = input(default=0.0d0,option=N_kg)
    obj%P_kg    = input(default=0.0d0,option=P_kg)
    obj%K_kg    = input(default=0.0d0,option=K_kg)
    obj%Ca_kg   = input(default=0.0d0,option=Ca_kg)
    obj%Mg_kg   = input(default=0.0d0,option=Mg_kg)
    obj%S_kg    = input(default=0.0d0,option=S_kg)
    obj%Fe_kg   = input(default=0.0d0,option=Fe_kg)
    obj%Mn_kg   = input(default=0.0d0,option=Mn_kg)
    obj%B_kg    = input(default=0.0d0,option=B_kg)
    obj%Zn_kg   = input(default=0.0d0,option=Zn_kg)
    obj%Mo_kg   = input(default=0.0d0,option=Mo_kg)
    obj%Cu_kg   = input(default=0.0d0,option=Cu_kg)
    obj%Cl_kg   = input(default=0.0d0,option=Cl_kg)

end subroutine
! ################################################################

! ################################################################
subroutine exportSoil(obj,FileName,format,objID)
    class(Soil_),intent(inout)::obj
    integer(int32),optional,intent(inout) :: objID
    character(*),intent(in)::FileName
    character(*),optional,intent(in) :: format

    if(present(format) )then
        if(format==".geo" .or. format=="geo" )then
            open(15,file=FileName)
            write(15,'(A)') "//+"
            write(15,'(A)') 'SetFactory("OpenCASCADE");'
            write(15,*) "Box(",input(default=1,option=objID),") = {",&
            obj%x   ,",", obj%y  ,",", obj%z ,   ",",&
            obj%width                ,",", obj%length               ,",", obj%depth ,"};"
            close(15)
            objID=objID+1
        endif
    endif
end subroutine
! ################################################################

subroutine diagnosisSoil(obj,FileName)
    class(Soil_),intent(inout) :: obj
    character(*),optional,intent(in)::FileName

    print *, "======================="
    print *, "Soil diagnosis"
    print *, "-----------------------"
    print *, "Total area ", trim(adjustl(fstring(obj%width*obj%length)))//" (cm^2)"
    print *, "Total area ",trim(adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0)))//" (m^2)"
    print *, "Total area ",trim(adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0/100.0d0)))//" (a)"
    print *, "Total area ",trim(adjustl(fstring(obj%width/100.0d0*obj%length/100.0d0/100.0d0/100.0d0)))//" (ha)"
    print *, "Total N  ",trim(adjustl(fstring(obj%N_kg )))//" (kg)"   
    print *, "Total P  ",trim(adjustl(fstring(obj%P_kg )))//" (kg)"   
    print *, "Total K  ",trim(adjustl(fstring(obj%K_kg )))//" (kg)"   
    print *, "Total Ca ",trim(adjustl(fstring(obj%Ca_kg)))//" (kg)"   
    print *, "Total Mg ",trim(adjustl(fstring(obj%Mg_kg)))//" (kg)"   
    print *, "Total S  ",trim(adjustl(fstring(obj%S_kg )))//" (kg)"   
    print *, "Total Fe ",trim(adjustl(fstring(obj%Fe_kg)))//" (kg)"   
    print *, "Total Mn ",trim(adjustl(fstring(obj%Mn_kg)))//" (kg)"   
    print *, "Total B  ",trim(adjustl(fstring(obj%B_kg )))//" (kg)"   
    print *, "Total Zn ",trim(adjustl(fstring(obj%Zn_kg)))//" (kg)"   
    print *, "Total Mo ",trim(adjustl(fstring(obj%Mo_kg)))//" (kg)"   
    print *, "Total Cu ",trim(adjustl(fstring(obj%Cu_kg)))//" (kg)"   
    print *, "Total Cl ",trim(adjustl(fstring(obj%Cl_kg)))//" (kg)"   
    print *, "-----------------------"
    print *, "Total N  ", trim(adjustl(fstring(obj%N_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"    
    print *, "Total P  ", trim(adjustl(fstring(obj%P_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"    
    print *, "Total K  ", trim(adjustl(fstring(obj%K_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"    
    print *, "Total Ca ", trim(adjustl(fstring(obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total Mg ", trim(adjustl(fstring(obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total S  ", trim(adjustl(fstring(obj%S_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"    
    print *, "Total Fe ", trim(adjustl(fstring(obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total Mn ", trim(adjustl(fstring(obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total B  ", trim(adjustl(fstring(obj%B_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"    
    print *, "Total Zn ", trim(adjustl(fstring(obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total Mo ", trim(adjustl(fstring(obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total Cu ", trim(adjustl(fstring(obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "Total Cl ", trim(adjustl(fstring(obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*1000.0)))//" (kg/10a)"   
    print *, "-----------------------"
    print *, "Total N  ",trim(adjustl(fstring(obj%N_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"  
    print *, "Total P  ",trim(adjustl(fstring(obj%P_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"  
    print *, "Total K  ",trim(adjustl(fstring(obj%K_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"  
    print *, "Total Ca ",trim(adjustl(fstring(obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total Mg ",trim(adjustl(fstring(obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total S  ",trim(adjustl(fstring(obj%S_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"  
    print *, "Total Fe ",trim(adjustl(fstring(obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total Mn ",trim(adjustl(fstring(obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total B  ",trim(adjustl(fstring(obj%B_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"  
    print *, "Total Zn ",trim(adjustl(fstring(obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total Mo ",trim(adjustl(fstring(obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total Cu ",trim(adjustl(fstring(obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)" 
    print *, "Total Cl ",trim(adjustl(fstring(obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10000.0d0)))//" (kg/ha)"    
    print *, "======================="

    if(present(FileName) )then
        open(16,file=FileName)
        
        write(16,*) "======================="
        write(16,*) "Soil diagnosis"
        write(16,*) "-----------------------"
        write(16,*) "Total N  (kg)",obj%N_kg    
        write(16,*) "Total P  (kg)",obj%P_kg    
        write(16,*) "Total K  (kg)",obj%K_kg    
        write(16,*) "Total Ca (kg)",obj%Ca_kg   
        write(16,*) "Total Mg (kg)",obj%Mg_kg   
        write(16,*) "Total S  (kg)",obj%S_kg    
        write(16,*) "Total Fe (kg)",obj%Fe_kg   
        write(16,*) "Total Mn (kg)",obj%Mn_kg   
        write(16,*) "Total B  (kg)",obj%B_kg    
        write(16,*) "Total Zn (kg)",obj%Zn_kg   
        write(16,*) "Total Mo (kg)",obj%Mo_kg   
        write(16,*) "Total Cu (kg)",obj%Cu_kg   
        write(16,*) "Total Cl (kg)",obj%Cl_kg   
        write(16,*) "-----------------------"
        write(16,*) "Total N  (kg/10a)",obj%N_kg /(obj%width/100.0d0)/(obj%length/100.0d0)    
        write(16,*) "Total P  (kg/10a)",obj%P_kg /(obj%width/100.0d0)/(obj%length/100.0d0)    
        write(16,*) "Total K  (kg/10a)",obj%K_kg /(obj%width/100.0d0)/(obj%length/100.0d0)    
        write(16,*) "Total Ca (kg/10a)",obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total Mg (kg/10a)",obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total S  (kg/10a)",obj%S_kg /(obj%width/100.0d0)/(obj%length/100.0d0)    
        write(16,*) "Total Fe (kg/10a)",obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total Mn (kg/10a)",obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total B  (kg/10a)",obj%B_kg /(obj%width/100.0d0)/(obj%length/100.0d0)    
        write(16,*) "Total Zn (kg/10a)",obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total Mo (kg/10a)",obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total Cu (kg/10a)",obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "Total Cl (kg/10a)",obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)   
        write(16,*) "-----------------------"
        write(16,*) "Total N  (kg/ha)",obj%N_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0    
        write(16,*) "Total P  (kg/ha)",obj%P_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0    
        write(16,*) "Total K  (kg/ha)",obj%K_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0    
        write(16,*) "Total Ca (kg/ha)",obj%Ca_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total Mg (kg/ha)",obj%Mg_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total S  (kg/ha)",obj%S_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0    
        write(16,*) "Total Fe (kg/ha)",obj%Fe_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total Mn (kg/ha)",obj%Mn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total B  (kg/ha)",obj%B_kg /(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0    
        write(16,*) "Total Zn (kg/ha)",obj%Zn_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total Mo (kg/ha)",obj%Mo_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total Cu (kg/ha)",obj%Cu_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0   
        write(16,*) "Total Cl (kg/ha)",obj%Cl_kg/(obj%width/100.0d0)/(obj%length/100.0d0)*10.0d0      
        write(16,*) "======================="
        close(16)
    endif
end subroutine


end module