use SPACClass
implicit none

type(SPAC_) :: spac
type(IO_) :: f

call f%download(from="https://plantfem.org/download/example.csv")
call f%download(from="https://plantfem.org/download/example.csv.condition.json")

! initialize
call spac%init(&
    csv_wave_file="example.csv",&
    json_metadata_file="example.csv.condition.json"&
    )
    
print *, "[Notice!] wave data format is ",spac%wave_data_format

! run SPAC
call spac%run()

! export as PDF-format
call spac%pdf(name="example_SPAC.pdf")


end