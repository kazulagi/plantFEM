from PIL import Image
import sys
import os
img_in = Image.open("/home/haruka/Dropbox/PlantSoil/parametric_study/incre_length_only_m/root_length_100per.svg.png")
python_buffer = open("GetPixcelByRGB_pid_1.txt","w")
python_buffer_size = open("GetPixcelByRGB_size_pid_1.txt","w")
rgb_im = img_in.convert('RGB')
size = rgb_im.size
print( str(size[0]), ' ',str(size[1])  )
itr = 0
width,height =img_in.size
for i in range(width):
   for j in range(height):
       R,G,B=rgb_im.getpixel((i,j))
       er=abs(R-0           )+abs(G-128         )+abs(B-0           )
       if er <= 5            :
           python_buffer.write(str(i)+'\t')
           itr=itr+1
           python_buffer.write(str(j)+'\n')
python_buffer_size.write(str(itr)+'\n')
img_in.close()
python_buffer.close()
python_buffer_size.close()
