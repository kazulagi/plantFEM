use SPACClass
implicit none

type(SPAC_) :: spac

character(50) :: fpath
character(:),allocatable :: filepath

!>>>> you can get example by >>>>>>
! plantfem dl SPAC_example.csv
! plantfem dl SPAC_example.csv.condition.json

call getarg(1,fpath)
filepath = trim(adjustl(fpath))

! initialize
call spac%init(&
    csv_wave_file=filepath,&
    json_metadata_file=filepath+".condition.json"&
    )
    
print *, "[Notice!] wave data format is ",spac%wave_data_format

! run SPAC
call spac%run()

! identify structure
!call spac%inversion(&
!    db="path/to/database/db_",&
!    db_shape=[74,200],&
!    frequency_scope=[3.20d0,30.0d0],&
!    name="inversion_list.csv")

! export as PDF-format
call spac%pdf(name="SPAC_report_"+filepath+".pdf")



end