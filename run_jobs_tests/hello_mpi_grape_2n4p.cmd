#PBS -S /usr/local/bin/bash
#PBS -q newest
#PBS -N hello_mpi_grape_2n4p
#PBS -o output
#PBS -e error
#PBS -l nodes=2:ppn=4
#PBS -l walltime=00:02:00

cd $PBS_O_WORKDIR
mpirun -np 8 hello_mpi_grape