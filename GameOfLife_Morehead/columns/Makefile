CC = mpif90

all:

gol_col: gol_subs.o gol_mpi_columns.o
	${CC} -o gol_col gol_subs.o gol_mpi_columns.o

gol_subs.o:
	${CC} -c gol_subs.f90

gol_mpi_columns.o:
	${CC} -c gol_mpi_columns.f90

clean:
	rm -f *.o *.mod gol_mpi *~