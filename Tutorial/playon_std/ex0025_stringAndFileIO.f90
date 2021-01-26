program main
    use std 
    implicit None

    type(IO_) :: file 
    type(String_) :: moji

    moji = "hello" 
    moji = moji + "From"
    moji = "A" + moji + str(moji)
    call print(moji)
    call file%open(moji+".txt")
    call file%write(moji)
    moji = moji + moji
    call file%write(moji)
    call file%close()
end program 