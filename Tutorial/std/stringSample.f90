use std
implicit none

type(String_) :: list(3,2)

list(1,1) = "soybean"; list(1,2) = "Glycine Max"
list(2,1) = "rice"   ; list(2,2) = "Oriza Sativa"
list(3,1) = "Alice"  ; list(3,2) = "in wonderland"


call print(list)

end