use plantfem
implicit none

type(IO_)   :: f ! to do something related to input-output
type(Time_) :: time ! to do something related to time
type(Soybean_) :: soy ! to do something related to soybean

! Loop
do
    if(.not.f%diff("soyconfig.json") )then
        ! if no diff in ./soyconfig.json => wait
        call time%sleep(10)
        call print("soyconfig.json is not updated yet.")
        cycle
    else
        ! if diff in ./soyconfig.json => update file
        call print("soyconfig.json is updated >> update soybean object.")
        ! initialize soybean object 
        call soy%init(config="soyconfig.json")
        ! export as stl file
        call soy%stl("tmp/soy")
        ! wait 10 sec.
        call time%sleep(10)
    endif
enddo

end