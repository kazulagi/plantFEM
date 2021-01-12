import os
import sys

Classlist=open("listOfClass.txt")
for line in Classlist:
    print(line)
    fn="../src/"+line.strip()+"/"+line.strip()+".f90"
    print(fn)
    ClassFile=open(fn)
    ofname=line.strip()+".txt"
    of=open(ofname,"w")
    read_all=0
    for SourceLine in ClassFile:
        print(SourceLine)
        SourceLine=ClassFile.readline()
        SourceLine=SourceLine.strip()
        if SourceLine.find("interface") == 0 :
            #SourceLine=SourceLine.strip("interface")
            #SourceLine=SourceLine.strip(" ")
            of.write(SourceLine+"\n")
        elif SourceLine.find("subroutine") == 0 :
            #SourceLine=SourceLine.strip("interface")
            #SourceLine=SourceLine.strip(" ")
            of.write(SourceLine+"\n")
        elif SourceLine.find("function") == 0 :
            #SourceLine=SourceLine.strip("interface")
            #SourceLine=SourceLine.strip(" ")
            of.write(SourceLine+"\n")
        elif SourceLine.find("type::") == 0:
            read_all=1
            of.write("Field :: \n")
        elif SourceLine.find("type( ") == 0:
            read_all=1
            of.write("Field :: \n")
            #of.write(SourceLine+"\n")
        elif SourceLine.find("type ") == 0:
            read_all=1
            of.write("Field :: \n")
        elif SourceLine.find("end type") == 0:
            read_all=0
            #of.write(SourceLine+"\n")
        elif SourceLine.find("contains") == 0:
            of.write("Methods :: \n")
        else:
            if read_all != 0:
                of.write(SourceLine+"\n")
            else:
                continue
    ClassFile.close()
    of.close()


Classlist.close()
    