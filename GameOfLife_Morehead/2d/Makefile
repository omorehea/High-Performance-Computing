CC = mpif90

all:

gol_2d: gol_subs.o gol_mpi_2D_decomp.o
	${CC} -o gol_2d gol_subs.o gol_mpi_2D_decomp.o

gol_subs.o:
	${CC} -c gol_subs.f90

gol_mpi_2D_decomp.o:
	${CC} -c gol_mpi_2D_decomp.f90

clean:
	rm -f *.o *.mod gol_mpi *~

