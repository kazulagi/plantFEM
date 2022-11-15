use SPACClass
implicit none

type(SPAC_) :: spac

! initialize
call spac%init(&
    csv_wave_file="sample.csv",&
    json_metadata_file="sample.csv.condition.json"&
    )
    
print *, "[Notice!] wave data format is ",spac%wave_data_format

! run SPAC
call spac%run()


! identify structure
call spac%inversion(&
    db="path/to/database/db_",&
    db_shape=[74,200],&
    frequency_scope=[3.20d0,30.0d0],&
    name="inversion_list.csv")

! export as PDF-format
call spac%pdf(name="sample_report.pdf")



end