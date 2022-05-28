README file for HW5. Describes how to COMPILE and RUN each program listed below.

-----------------------------------------------------------

-- hello_world_omp.f90 --

Compile: gfortran -fopenmp hello_world_omp.f90 -o hello_world_omp

Declare Number of Threads (in bash): export OMP_NUM_THREADS=8

Run: ./hello_world_omp

-----------------------------------------------------------

-- matprod_omp.f90 --

Compile: gfortran -fopenmp matprod_omp.f90 -o matprod_omp

Declare Number of Threads (in bash): export OMP_NUM_THREADS=10

Run: ./matprod_omp

-----------------------------------------------------------