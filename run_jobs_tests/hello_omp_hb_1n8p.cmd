#!/bin/csh                                                                                                    

#SBATCH -p Instruction                                                                                                       
#SBATCH -J hello_omp_hb_1n8p                                                                                                 
#SBATCH -e error/hb_omp_1n8p_%j.err                                                                                          
#SBATCH -o output/hb_omp_1n8p_%j.out                                                                                         
#SBATCH -N 1                                                                                                                 
#SBATCH -c 4                                                                                                  
#SBATCH -t 00:05:00                                                                                                          

setenv OMP_NUM_THREADS 8
./hello_omp_hb
