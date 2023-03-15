#!/usr/bin/gnuplot
reset

FILE = "../data/polygon2.dat"

set size ratio -1
set style fill solid 0.3
set grid x,y front
set key noautotitle
set grid
set title "SuperElement"

stats FILE u 0 nooutput   # get number of blocks, i.e. polygons
N = STATS_blocks

getArea(colX,colY)   = ($0==0?(Area=0, x1=column(colX), y1=column(colY)) : 0, \
                        x0=x1, y0=y1, x1=column(colX), y1=column(colY), Area=Area+0.5*(y1+y0)*(x1-x0))
getMinMax(colX,colY) = (x2=column(colX), y2=column(colY), $0==0? (xMin=xMax=x2, yMin=yMax=y2) : \
                       (x2<xMin?xMin=x2:0, x2>xMax?xMax=x2:0, y2<yMin?yMin=y2:0, y2>yMax?yMax=y2:0))
Areas = Centers      = ''
do for [i=1:N] {
    stats FILE u (getArea(1,2),getMinMax(1,2)) index i-1 nooutput
    Areas   = Areas.sprintf(" %g",abs(Area))
    Centers = Centers.sprintf(' %g %g',0.5*(xMin+xMax),0.5*(yMin+yMax))

}
indexf(i) = i
CenterX(n) = real(word(Centers,int(column(n))*2+1))
CenterY(n) = real(word(Centers,int(column(n))*2+2))
Area(n)    = real(word(Areas,int(column(n)+1)))
myColors   = "0xff0000 0x00ff00 0x0000ff"
color = "0xff0000"
myColor(i) = color
set linetype 1 lc rgb "black"
plot for [i=1:N] FILE u 1:2 index i-1 w filledcurves lc rgb myColor(i) border lt 1 lw 3 ,\
     '+' u (CenterX(0)):(CenterY(0)):(sprintf("A=%g",(0)))) every ::0::N-1 w labels center
     
### end of script
