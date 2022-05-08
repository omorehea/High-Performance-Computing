FC = gfortran

FFLAGS = -Wall -Wextra -Wno-surprising -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace -fdefault-real-8 -fdefault-double-8 

OBJECTS = funct.o trap_int.o

MODULES = trap_int.mod

trap_int: $(OBJECTS)
	$(FC) -o $@ $^ $(FFLAGS)

output.txt: trap_int.exe
	./trap_int.exe > output.txt

trap_int.exe: $(MODULES) $(OBJECTS)
	gfortran $(OBJECTS) -o trap_int.exe

%.o: %.f90
	$(FC) -c $< $(FFLAGS)

%.mod: %.f90
	gfortran -c -fdefault-real-8 -fdefault-double-8  $<
.PHONY: clean

clean:
	rm -f $(OBJECTS) $(MODULES) trap_int  trap_int.exe *.o *.mod *~
