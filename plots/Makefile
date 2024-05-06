MAKEFILE_PATH = $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR = $(dir $(MAKEFILE_PATH))


all :
	@echo "Makefile is in: $(CURRENT_DIR)"
	make all -C lagrangeShapeFct_plot
	make all -C lagrangeShapeFctDeriv_plot
	make all -C superElement_plot

gnuplot :
	make gnuplot -C lagrangeShapeFct_plot

python :
	make python -C superElement_plot

clean:
	make clean -C lagrangeShapeFct_plot
	make clean -C lagrangeShapeFctDeriv_plot
	make clean -C superElement_plot