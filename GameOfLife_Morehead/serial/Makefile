FC = gfortran

FFLAGS = -Wall -Wextra -Wimplicit-interface -Wno-surprising -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace -fdefault-real-8 -fdefault-double-8 

OBJECTS = gol_serial.o

MODULES = gol_serial.mod

gol_serial: $(OBJECTS)
	$(FC) -o $@ $^ $(FFLAGS)

output.txt: gol_serial.exe
	./gol_serial.exe > output.txt

gol_serial.exe: $(MODULES) $(OBJECTS)
	gfortran $(OBJECTS) -o gol_serial.exe

%.o: %.f90
	$(FC) -c $< $(FFLAGS)

%.mod: %.f90
	gfortran -c -fdefault-real-8 -fdefault-double-8  $<
.PHONY: clean

clean:
	rm -f $(OBJECTS) $(MODULES) gol_serial  gol_serial.exe *.o *.mod *~
