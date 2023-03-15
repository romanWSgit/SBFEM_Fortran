GNUPLOTIMAGES = ../images/lagrangeShapeFct.pdf
PYTHONIMAGES = ../images/lagrangeShapeFct_py.svg

IMAGES=${GNUPLOTIMAGES} \
	   ${PYTHONIMAGES}

all : $(IMAGES)

gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm $(IMAGES)
	
	
${GNUPLOTIMAGES}: gnuplot/plot_lagrangeShapeFct.gnuplot ../data/lagrangeShapeFct.dat
	gnuplot gnuplot/plot_lagrangeShapeFct.gnuplot

${PYTHONIMAGES}: python/plot_lagrangeShapeFct.py ../data/lagrangeShapeFct.dat
	python python/plot_lagrangeShapeFct.py