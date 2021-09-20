import os

os.system("ls Tutorial/app/*/*.f90 > auto_test_list.txt")
os.system("ls Tutorial/app/*.f90 >> auto_test_list.txt")
os.system("ls Tutorial/std/*.f90 >> auto_test_list.txt")
os.system("ls Tutorial/fem/*.f90 >> auto_test_list.txt")
os.system("ls Tutorial/sim/*.f90 >> auto_test_list.txt")
os.system("ls Tutorial/obj/*.f90 >> auto_test_list.txt")

with open("auto_test_list.txt","r") as f:
    for line in f:
        print(line)
        err = os.system("plantfem load "+line)
        err = os.system("mpirun --allow-run-as-root ./server.out ")
        if err < 1:
            print("[ok] Built "+line+" > pass")
        else:
            print("[ERROR] Built "+line+" ERROR detected.")
            break
