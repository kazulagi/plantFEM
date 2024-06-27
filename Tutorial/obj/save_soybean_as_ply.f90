use plantfem

type(Soybean_) :: soybean

call soybean%create("Tutorial/obj/soy.json")
call soybean%ply("soy.ply",single_file=True)

end