#!/bin/csh                                                                                                    

#SBATCH -p Instruction                                                                                                       
#SBATCH -J hello_omp_hb_1n4p                                                                                                 
#SBATCH -e error/hb_omp_1n4p_%j.err                                                                                          
#SBATCH -o output/hb_omp_1n4p_%j.out                                                                                         
#SBATCH -N 1                                                                                                                 
#SBATCH -c 4                                                                                                  
#SBATCH -t 00:05:00                                                                                                          

setenv OMP_NUM_THREADS 4
./hello_omp_hb
