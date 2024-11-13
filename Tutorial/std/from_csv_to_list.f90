use IOClass
implicit none

type(List_) :: data_list
type(IO_) :: f

call f%open("test.txt","w")
call f%write("abcdefg, hijklmn, op, qrstu")
call f%write("abcd, hin, opdd, qrstu")
call f%write("cd, h, op, ddddqrstu")
call f%close()


data_list = f%to_list("test.txt",column=1,separator=",",header=0)

print *, str(data_list)

end