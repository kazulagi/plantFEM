from PIL import Image, ImageSequence
import numpy as np
import cv2

import sys, os
from PIL import Image
img_all = Image.open("your.tiff",'r')

itr = ImageSequence.Iterator(img_all)

pixelPosition = open("pixelPosition.txt","w")

interval = 1

for i in range(10000):
    try:
        if i%interval != 0 :
            continue
        print("Frame : " + str(i))
        img = itr[i] # 2ページ目
        img = np.array(img)

    
        edge = cv2.Canny(img,100,400)

        cv2.imwrite('ret'+ str(i)+ '.png', edge)
        
        # ピクセル処理
        size = img.size
        for j in range( img.shape[0] ):
            for k in range( img.shape[0] ):
                color = img[j,k]
                if(color > 100):
                    print(str(i) + " " + str(j) + " " + str(k) )
                    pixelPosition.write(str(i) + " " + str(j) + " " + str(k)+"\n" )
    except IndexError:
        print("end")
        break