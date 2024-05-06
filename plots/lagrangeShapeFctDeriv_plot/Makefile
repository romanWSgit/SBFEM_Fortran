MAKEFILE_PATH = $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR = $(dir $(MAKEFILE_PATH))

GNUPLOTIMAGES = ${CURRENT_DIR}../images/lagrangeShapeFctDeriv.pdf
PYTHONIMAGES = ${CURRENT_DIR}../images/lagrangeShapeFctDeriv_py.svg

IMAGES=${GNUPLOTIMAGES} \
	   ${PYTHONIMAGES}

all : $(IMAGES)
	@echo "The working directory of the lagrangeShapeFctDeriv_plot - makefile is: "${CURRENT_DIR}

gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm -f $(IMAGES)
	
	
${GNUPLOTIMAGES}: ${CURRENT_DIR}gnuplot/plot_lagrangeShapeFctDeriv.gnuplot ${CURRENT_DIR}../data/lagrangeShapeFctDeriv.dat
	gnuplot -e "script_path='${CURRENT_DIR}gnuplot/'" gnuplot/plot_lagrangeShapeFctDeriv.gnuplot

${PYTHONIMAGES}: ${CURRENT_DIR}python/plot_lagrangeShapeFctDeriv.py ${CURRENT_DIR}../data/lagrangeShapeFctDeriv.dat
	python3 python/plot_lagrangeShapeFctDeriv.py