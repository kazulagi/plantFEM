import json
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# 以下、matplotlibで描画
#x軸とy軸にラベル付け
fig = plt.figure()
ax = Axes3D(fig)

ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("z")

# read file
json_file = open('soy2.json', 'r')
json_object = json.load(json_file)

# get number of stem, leaf and root
num_leaf=int(json_object["num_leaf"])
num_stem=int(json_object["num_stem"])
num_root=int(json_object["num_root"])

# メッシュオブジェクト中の節点座標配列を取り出す
for i in range(num_leaf):
    leaf = 'leaf' + str(i+1)
    nodcoord = np.array(json_object[leaf]["mesh"]["NodCoord"])
    # 節点を描画
    x = nodcoord[:,0]
    y = nodcoord[:,1]
    z = nodcoord[:,2]
    ax.scatter(x,y,z,marker="*",linestyle='solid',color="g",s=0.01)
for i in range(num_stem):
    stem = 'stem' + str(i+1)
    nodcoord = np.array(json_object[stem]["mesh"]["NodCoord"])
    # 節点を描画
    x = nodcoord[:,0]
    y = nodcoord[:,1]
    z = nodcoord[:,2]
    ax.scatter(x,y,z,marker="*",linestyle='solid',color="y",s=0.01)
for i in range(num_root):
    root = 'root' + str(i+1)
    nodcoord = np.array(json_object[root]["mesh"]["NodCoord"])
    # 節点を描画
    x = nodcoord[:,0]
    y = nodcoord[:,1]
    z = nodcoord[:,2]
    ax.scatter(x,y,z,marker="*",linestyle='solid',color="k",s=0.01)

# 図を表示
plt.xlim([-1,1])
plt.ylim([-1,1])
plt.show()