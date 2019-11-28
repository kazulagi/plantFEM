from PIL import Image
import sys
import os
img_in = Image.open("/home/haruka/Dropbox/PlantSoil/parametric_study/incre_length_only_m/root_length_100per.svg.png")
python_buffer = open("GetPixcelSize_pid_1.txt","w")
rgb_im = img_in.convert('RGB')
size = rgb_im.size
print( str(size[0]), ' ',str(size[1])  )
python_buffer.write( str(size[0]))
python_buffer.write('\n')
python_buffer.write( str(size[1]))
img_in.close()
python_buffer.close()
