import json
import os

# convert server.json into server.f90

server_json = open('server.json', 'r')
server = json.load(server_json)

server_f90 = open('server.f90','w')

server_f90.write('program main\n')
server_f90.write("  ")
server_f90.write('use plantFEM\n')
# declear variables
server_f90.write("  ")
server_f90.write("type(")
server_f90.write(str(server["object"]["type"]) )
server_f90.write(") :: ")
server_f90.write(str(server["object"]["name"]))
server_f90.write("\n")

# Operations
if "create" in str(server["object"]["exec"]):
    server_f90.write("  ")
    server_f90.write("call "+str(server["object"]["name"])+"%create(")
    # arguments
    if "meshtype" in str(server["object"]["exec"]["create"]):
        server_f90.write('meshtype='+"'"+str(server["object"]["exec"]["create"]["meshtype"])+"'")
    server_f90.write(")\n")

# Operations
if "msh" in str(server["object"]["exec"]):
    server_f90.write("  ")
    server_f90.write("call "+str(server["object"]["name"])+"%msh(")
    # arguments
    if "name" in str(server["object"]["exec"]["msh"]):
        server_f90.write('name='+"'"+str(server["object"]["exec"]["msh"]["name"])+"'")
    server_f90.write(")\n")

    #server_f90.write("call "+str(server["object"]["name"])+"%msh(")
    #if "name" in str(server["object"]["exec"]["msh"]):
    #    server_f90.write('name='+"'"+str(server["object"]["exec"]["msh"]["name"])+"'")
    #server_f90.write(")")
server_f90.write('end program main\n')
server_f90.close()
os.system("./plantfem run")
