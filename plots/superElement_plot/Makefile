GNUPLOTIMAGES = ../images/superElement.pdf
PYTHONIMAGES = ../images/superElement_py.svg

IMAGES=${GNUPLOTIMAGES} ${PYTHONIMAGES}

all : $(IMAGES)

gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm $(IMAGES)
	
	
${GNUPLOTIMAGES}: gnuplot/plot_superElement.gnuplot 
	gnuplot gnuplot/plot_superElement.gnuplot

${PYTHONIMAGES}: python/plot_superElement.py ../data/polygon.dat ../data/nodes.dat
	python python/plot_superElement.py