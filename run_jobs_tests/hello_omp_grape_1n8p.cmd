#PBS -S /usr/local/bin/bash
#PBS -q newest
#PBS -N hello_omp_grape_1n8p
#PBS -o output
#PBS -e error
#PBS -l nodes=1:ppn=8
#PBS -l walltime=00:02:00

cd $PBS_O_WORKDIR
export OMP_NUM_THREADS=8
./hello_omp_grape