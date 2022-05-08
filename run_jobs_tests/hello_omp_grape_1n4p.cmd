#PBS -S /usr/local/bin/bash
#PBS -q newest
#PBS -N hello_omp_grape_1n4p
#PBS -o output
#PBS -e error
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:02:00

cd $PBS_O_WORKDIR
export OMP_NUM_THREADS=4
./hello_omp_grape