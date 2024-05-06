MAKEFILE_PATH = $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR = $(dir $(MAKEFILE_PATH))

GNUPLOTIMAGES = ${CURRENT_DIR}../images/superElement.pdf
PYTHONIMAGES = ${CURRENT_DIR}../images/superElement_py.svg

IMAGES=${GNUPLOTIMAGES} \
	   ${PYTHONIMAGES}

all : $(IMAGES)

gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm -f $(IMAGES)
	
	
${GNUPLOTIMAGES}: gnuplot/plot_superElement.gnuplot ${CURRENT_DIR}../data/polygon.dat ${CURRENT_DIR}../data/nodes.dat
	gnuplot -e "script_path='${CURRENT_DIR}gnuplot/'" gnuplot/plot_superElement.gnuplot

${PYTHONIMAGES}: ${CURRENT_DIR}python/plot_superElement.py ${CURRENT_DIR}../data/polygon.dat ${CURRENT_DIR}../data/nodes.dat
	python3 python/plot_superElement.py