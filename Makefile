SRCDIR = src
APPDIR = app
OBJDIR = obj
BINDIR = bin

MODULES = functional.f90 helper_functions.f90 \
			math.f90  sbfem_functions.f90 plots.f90\
			SbfemClass.f90 
MAIN = sbfem.f90

OBJMODULES = $(MODULES:%.f90=$(OBJDIR)/%.o)

OBJMAIN = $(OBJDIR)/sbfem.o

PROGRAM = $(BINDIR)/sbfem

FC = gfortran
FFLAGS =  -O3 -std=f2008 
AD_FFLAGS =-O3 -std=f2008 -I include/
AD_LINK_FFLAGS =-O3 -std=f2008 -I include/ lib/libjsonfortran.a

all: $(PROGRAM)

$(OBJDIR)/helper_functions.o: $(SRCDIR)/helper_functions.f90
	$(FC) $(AD_FFLAGS) -c $< -o $@

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJDIR)/%.o: $(APPDIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(PROGRAM): $(OBJMODULES) $(OBJMAIN)
	$(FC) $(AD_LINK_FFLAGS) $^ -o $@
	@#ford docs.md


.PHONY: all clean

clean:
	@rm -f $(OBJDIR)/*.o $(PROGRAM)
	@rm -f *.mod *.o sbfem 
	@(cd plots && make clean ) 
