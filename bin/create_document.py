
fname = "src/FEMDomainClass/FEMDomainClass.f90"
document = open("doc.md","w")

with open(fname,"r") as f:
    type_zone = 0
    for line in f:
        if("type" in line and "::" in line):
            if  not ( "(" in line ) :
                if not "char" in line:
                    if not "int" in line and not "real" in line:
                        type_zone = 1
            
        if("end" in line and  "type" in line  ):
            type_zone = 0
            

        if("contains" in line):
            break

        if type_zone==1:
            document.write("## Class name :: ")
            line = str(line)
            line=line.replace("type","")
            line=line.replace("type","")
            line=line.replace("::","")
            line=line.replace("::","")
            line=line.replace(" ","")
            document.write( line + "\n")
            document.write( "### Member variavles\n" )
            print(line)
            
        elif type_zone==2:
            document.write(line)
        
        if type_zone == 1:
            type_zone = 2


