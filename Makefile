# Standard Makefile for Fortran the SBFEM project structure

SRCDIR = src
APPDIR = app
OBJDIR = obj
BINDIR = bin
TESTDIR = tests
TESTSRC = $(wildcard $(TESTDIR)/test-drive/src/*.f90) $(wildcard $(TESTDIR)/test-drive/test/*.f90)

COPY_MODS = no


MODULES = functional.f90 helper_functions.f90 \
			math.f90  sbfem_functions.f90 SbfemClass.f90 \
			plots.f90
		
MAIN = sbfem.f90

OBJMODULES = $(MODULES:%.f90=$(OBJDIR)/%.o)

OBJMAIN = $(OBJDIR)/sbfem.o

PROGRAM = $(BINDIR)/sbfem

TESTOBJ = $(TESTSRC:.f90=.o)

FC = gfortran
FFLAGS =  -O3 -std=f2008  -J$(OBJDIR) -Iinclude/
AD_FFLAGS =-O3 -std=f2008 -Iinclude/ -J$(OBJDIR)
AD_LINK_FFLAGS =-O3 -std=f2008 -Llib -ljsonfortran
#AD_LINK_FFLAGS =-O3 -std=f2008 -Iinclude/ lib/libjsonfortran.a


all: $(PROGRAM)

$(OBJDIR)/helper_functions.o: $(SRCDIR)/helper_functions.f90
	$(FC) $(AD_FFLAGS) -c $< -o $@
	@if [ "$(COPY_MODS)" = "yes" ]; then \
		cp $(OBJDIR)/*.mod $(SRCDIR)/; \
		cp $(OBJDIR)/*.mod $(APPDIR)/; \
	fi
	


$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@
	@if [ "$(COPY_MODS)" = "yes" ]; then \
		cp $(OBJDIR)/*.mod $(SRCDIR)/; \
		cp $(OBJDIR)/*.mod $(APPDIR)/; \
	fi
$(OBJDIR)/%.o: $(APPDIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@
	@if [ "$(COPY_MODS)" = "yes" ]; then \
		cp $(OBJDIR)/*.mod $(SRCDIR)/; \
		cp $(OBJDIR)/*.mod $(APPDIR)/; \
	fi

$(TESTOBJ)/%.o: $(TESTSRC)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(PROGRAM): $(OBJMODULES) $(OBJMAIN)
	$(FC) $(AD_LINK_FFLAGS) $^ -o $@
	@if [ "$(COPY_MODS)" = "yes" ]; then \
		cp include/*.mod $(SRCDIR)/; \
		cp include/*.mod  $(APPDIR)/; \
	fi
	@#ford docs.md

test: $(TESTOBJ) $(OBJ)
	@echo "src: " $(SRCDIR)
	$(FC) $(FLAGS) -o run_tests $^
	./run_tests                 # Execute the test binary


.PHONY: all clean test

clean:
	@echo "Cleaning up..."
	@echo ""
	@echo "..."
	@echo ""
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.mod $(SRCDIR)/*.o $(SRCDIR)/*.mod $(APPDIR)/*.o $(PROGRAM) $(APPDIR)/*.mod 
	rm -f $(APPDIR)/libjsonfortran.a $(SRCDIR)/libjsonfortran.a
#	@rm -f *.mod *.o sbfem 
	@(cd plots && make clean ) 
	@echo ""
	@echo "..."
	@echo ""
	@echo "Cleaned up!"