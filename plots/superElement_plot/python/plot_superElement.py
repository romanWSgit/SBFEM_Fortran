

import numpy as np
import matplotlib.pyplot as plt
from sys import platform
import os
from pathlib import Path
wd = os.getcwd()



if platform == 'win32':
    unicodeVar = 'utf-16'
else:
    unicodeVar = 'utf-8'



# read data into a list
elements_file = "/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots/data/polygon.dat"
nodes_file = "/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots/data/nodes.dat"
#input_file = str(Path(wd).parents[1]) +"/data/lagrangeShapeFct.dat"
if platform == 'win32':
    unicodeVar = 'utf-16'
else:
    unicodeVar = 'utf-8'
# read data into a list
data = [line.rstrip().split() for line in open(elements_file, encoding = unicodeVar).readlines()]
data = np.array(data)
points1 = data[:,0:2]
points2 = data[:,2:4]
points3 = data[:,4:6]
np.shape(data)
ielem = np.shape(data)[0]

node_data = [line.rstrip().split() for line in open(nodes_file, encoding = unicodeVar).readlines()]
nodedim = np.shape(node_data)[0]
nodes = np.array(node_data)

centre = np.array([0.0,0.0])


polygonList = []
for i in range(0, ielem):
    polygonList.append(
        plt.Polygon([points1[i],points2[i],points3[i]],
                    fc='y', edgecolor='r'))
    plt.gca().add_patch(polygonList[i])
for i in range(nodedim):
    plt.text(float(nodes[i,0]), float(nodes[i,1]), i+1,fontweight='bold')
plt.text(centre[0], centre[1], "C", color='green', fontweight='bold')
plt.axis('scaled')










plt.title("SuperElement")
# plt.legend(legend_arr,bbox_to_anchor=(1.04, 0.5), loc="center left")
# plt.xlabel("x")
# plt.ylabel("f(x)")
# plt.grid(True, linestyle='--')
# plt.xlim([-1, 1])
plt.savefig("/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots/images/superElement_py.svg",bbox_inches="tight")
#plt.show()

