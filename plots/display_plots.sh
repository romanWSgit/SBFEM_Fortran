#!/bin/sh

# This program displays the
# following pictures:
# 
#FOLDER_PATH="/Users/roman_w_s/Developer/FORTRAN/SBFEM/plots"

# Access the folder
#cd $FOLDER_PATH
cd ../plots
make all
make all
open  ./images/lagrangeShapeFct.pdf
open  ./images/lagrangeShapeFct_py.svg
open  ./images/lagrangeShapeFctDeriv.pdf
open  ./images/lagrangeShapeFctDeriv_py.svg
open  ./images/superElement.pdf
open  ./images/superElement_py.svg

