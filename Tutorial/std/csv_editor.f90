use DictionaryClass
implicit none

type(Dictionary_) :: df,df_copy

df = dict()
!
do i_i=1,100
    call df%update("DataFrame_"+str(i_i), i_i )
enddo

! to show
call df%show()

call df%to_csv("filename")
call df%to_json("filename")

df_copy = to_dict("filename.csv")
call df_copy%update("DataFrame_98",12345)
call df_copy%to_csv("filename2")


end
