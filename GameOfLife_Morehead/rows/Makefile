CC = mpif90

all:

gol_row: gol_subs.o gol_mpi_rows.o
	${CC} -o gol_row gol_subs.o gol_mpi_rows.o

gol_subs.o:
	${CC} -c gol_subs.f90

gol_mpi_rows.o:
	${CC} -c gol_mpi_rows.f90

clean:
	rm -f *.o *.mod gol_mpi *~

