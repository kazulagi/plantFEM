

MeshClass.o: MeshClass.h MeshClass.cpp
	g++ -I./ -c MeshClass.h MeshClass.cpp 

test:MeshClass.o test.cpp
	g++ -I./ MeshClass.o test.cpp -o test

clean: rm -f MeshClass.o test