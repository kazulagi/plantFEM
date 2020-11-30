import json
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

json_file = open('domain.json', 'r')
json_object = json.load(json_file)

# メッシュオブジェクト中の節点座標配列を取り出す

nodcoord = np.array(json_object["mesh"]["NodCoord"])

# 以下、matplotlibで描画
#x軸とy軸にラベル付け
fig = plt.figure()
ax = Axes3D(fig)

ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("z")

# 節点を描画

x = nodcoord[:,0]
y = nodcoord[:,1]
z = nodcoord[:,2]
ax.plot(x,y,z,marker="o",linestyle='None')

# 図を表示

plt.show()