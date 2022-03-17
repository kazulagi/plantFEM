use DictionaryClass
implicit none

type(Dictionary_) :: d1

d1 = dict()
call d1 % update("Year",2022)
call d1 % update("pi",3.0d0)

! overwrite
call d1 % update("Year",2023)
call d1 % update("pi",3.141590d0)

! show
call d1 % show()

print *, d1 % find("pi")

end