# check end of script 
# if "end" nor "end program" are not present, then
# add "end"
import os

check1=0
check2=0
with open('server.f90', mode='rt', encoding='utf-8') as f:
    read_data = f.readlines()
    #remspace = str(read_data).replace(' ','')
    #remspace = str(remspace).replace('\n','')
    #print(str(read_data))
    if "end program" in str(read_data):
        check1=1
    if "endprogram\n" in read_data:
        check1=1
    if "end" in read_data:
        check2=1
    if "end\n" in read_data:
        check2=1

if check1+check2==0:
    os.system("echo 'end' >> server.f90")