import numpy as np


# 当然、型宣言はなし。
#
#
#
#
#


# 乱数入り配列の作成
dat = np.random.randn(3,5)

# 配列の表示
print(dat)

# 計算＋配列の表示
print(dat * 10) 
print(dat + 2*dat) 

# 配列サイズの表示
print(dat.shape) 

#  配列の書き出し
np.savetxt("./array1.txt",dat)

# 配列の読み込み
dat1 = np.loadtxt("./array1.txt")


#  配列の表示
print(dat1)
