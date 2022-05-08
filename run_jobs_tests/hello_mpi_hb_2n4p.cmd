#!/bin/bash

#SBATCH -p Instruction
#SBATCH -J hello_mpi_hb_2n4p
#SBATCH -e error/hb_mpi_2n4p_%j.err
#SBATCH -o output/hb_mpi_2n4p_%j.out
#SBATCH -N 2
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:05:00

mpirun -np 8 hello_mpi_hb