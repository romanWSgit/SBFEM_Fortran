

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
input_file = "/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots/data/lagrangeShapeFct.dat"
#input_file = str(Path(wd).parents[1]) +"/data/lagrangeShapeFct.dat"
if platform == 'win32':
    unicodeVar = 'utf-16'
else:
    unicodeVar = 'utf-8'
# read data into a list
data = [line.rstrip().split() for line in open(input_file, encoding = unicodeVar).readlines()]

np.shape(data)
shape_fcts = np.shape(data)[1]


x = [float(line[0]) for line in data]
h =[[float(line[i]) for line in data] for i in range(1,shape_fcts)]

legend_arr = [("shape " + str(i)) for i in range(1,shape_fcts + 2)]
plt.plot(x,np.transpose(h))
plt.title("Lagrangian Shape functions")
plt.legend(legend_arr,bbox_to_anchor=(1.04, 0.5), loc="center left")
plt.xlabel("x")
plt.ylabel("f(x)")
plt.grid(True, linestyle='--')
plt.xlim([-1, 1])
plt.savefig("/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots/images/lagrangeShapeFct_py.svg",bbox_inches="tight")


