### plot some colored triangles from a datablock
reset session

set terminal pdfcairo enhanced color dashed font "Alegreya, 14" \
rounded size 16 cm, 9.6 cm

FileToDatablock(f,d) = GPVAL_SYSNAME[1:7] eq "Windows" ? \
                       sprintf('< echo   %s ^<^<EOD  & type "%s"',d,f) : \
                       sprintf('< echo "\%s   <<EOD" & cat  "%s"',d,f)     # Linux/MacOS

FILE = "../data/polygon.dat"
load FileToDatablock(FILE,'$Data')

set output '../images/superElement.pdf'
set title "superElement"
### draw some colored triangles from a datablock




vx(n,t)  = word($Data[n],t*2-1)     # vertex x-coordinate
vy(n,t)  = word($Data[n],t*2)       # vertex y-coordinate
color(n) = "0xff0000"         # triangle color

set linetype 1 lc rgb "black"
do for [n=1:|$Data|] {
    set obj n polygon from vx(n,1),vy(n,1) to vx(n,2),vy(n,2) to vx(n,3),vy(n,3) to vx(n,1),vy(n,1)
    set obj n fc rgb color(n) fs solid 0.5 border lt 1 lw 3
}

#set size square

set xrange[-13:13]
set yrange[-6:6]
set size ratio -1
#set autoscale
plot NaN notitle    # or plot something else
### end of script

# vx(n,t)  = word($Data[n],t*2-1)     # vertex x-coordinate
# vy(n,t)  = word($Data[n],t*2)       # vertex y-coordinate
# color(n) = "0xff0000"         # triangle color

# set print $Triangles
#    do for [n=1:|$Data|]  {
#        print sprintf("%s %s %s\n%s %s 0\n%s %s 0\n%s %s 0\n\n", \
#                      vx(n,1),vy(n,1),color(n), vx(n,2),vy(n,2), vx(n,3),vy(n,3), vx(n,1),vy(n,1))
#    }
# set print

# set size square
# #set xrange[0:5]
# #set yrange[0:5]
# set linetype 1 lc rgb "black" lw 3
# set style fill solid 0.5

# plot $Triangles u 1:2:3 w filledcurves lc rgb var notitle
# ### end of script
