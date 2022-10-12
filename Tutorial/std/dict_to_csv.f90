use DictionaryClass
implicit none

type(Dictionary_) :: df
df = dict()

do i_i=1,100
    call df%update("DataFrame_"+str(i_i), i_i )
enddo

call df%show()

print *, fint(df%find("DataFrame_22")) + 1

call df%to_csv("filename")
call df%to_json("filename")


end