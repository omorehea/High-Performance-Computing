FC = gfortran

FFLAGS = -Wall -Wextra -Wimplicit-interface -Wno-surprising -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace -fdefault-real-8 -fdefault-double-8 

OBJECTS = ones.o

MODULES = ones.mod

ones: $(OBJECTS)
	$(FC) -o $@ $^ $(FFLAGS)

output.txt: ones.exe
	./ones.exe > output.txt

ones.exe: $(MODULES) $(OBJECTS)
	gfortran $(OBJECTS) -o ones.exe

%.o: %.f90
	$(FC) -c $< $(FFLAGS)

%.mod: %.f90
	gfortran -c -fdefault-real-8 -fdefault-double-8  $<
.PHONY: clean

clean:
	rm -f $(OBJECTS) $(MODULES) ones  ones.exe *.o *.mod *~
