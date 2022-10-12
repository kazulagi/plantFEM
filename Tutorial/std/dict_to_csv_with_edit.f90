use DictionaryClass
use RandomClass
implicit none

type(Dictionary_) :: df,df_copy
type(Random_)     :: random

df = dict()
!
do i_i=1,100
    call df%update("2022/01/01/"+zfill(i_i,4), random%gauss(mu=0.0d0,sigma=1.0d0) )
enddo

! to show
call df%show()

call df%to_csv("filename")
call df%to_json("filename")

df_copy = to_dict("filename.csv")

do i_i=20,30
    call df_copy%update("2022/01/01/"+zfill(i_i,4),0.0d0)
enddo

call df_copy%to_csv("filename2")


end
