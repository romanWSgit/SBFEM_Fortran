GNUPLOTIMAGES = ../images/lagrangeShapeFctDeriv.pdf
PYTHONIMAGES = ../images/lagrangeShapeFctDeriv_py.svg

IMAGES=${GNUPLOTIMAGES} \
	   ${PYTHONIMAGES}

all : $(IMAGES)

gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm $(IMAGES)
	
	
${GNUPLOTIMAGES}: gnuplot/plot_lagrangeShapeFctDeriv.gnuplot ../data/lagrangeShapeFctDeriv.dat
	gnuplot gnuplot/plot_lagrangeShapeFctDeriv.gnuplot

${PYTHONIMAGES}: python/plot_lagrangeShapeFctDeriv.py ../data/lagrangeShapeFctDeriv.dat
	python python/plot_lagrangeShapeFctDeriv.py