MAKEFILE_PATH = $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR = $(dir $(MAKEFILE_PATH))

GNUPLOTIMAGES =  ${CURRENT_DIR}../images/lagrangeShapeFct.pdf
PYTHONIMAGES =  ${CURRENT_DIR}../images/lagrangeShapeFct_py.svg



IMAGES=${GNUPLOTIMAGES} \
	   ${PYTHONIMAGES}

all : $(IMAGES)
	@echo "The working directory of the lagrangeShapeFct_plot - makefile is: "${CURRENT_DIR}	
	
gnuplot : $(GNUPLOTIMAGES)

python : $(PYTHONIMAGES)

clean:
	rm -f $(IMAGES)
	
	
${GNUPLOTIMAGES}: ${CURRENT_DIR}gnuplot/plot_lagrangeShapeFct.gnuplot  ${CURRENT_DIR}../data/lagrangeShapeFct.dat
	gnuplot -e "script_path='${CURRENT_DIR}gnuplot/'" gnuplot/plot_lagrangeShapeFct.gnuplot

${PYTHONIMAGES}: ${CURRENT_DIR}python/plot_lagrangeShapeFct.py  ${CURRENT_DIR}../data/lagrangeShapeFct.dat
	python3 ${CURRENT_DIR}python/plot_lagrangeShapeFct.py