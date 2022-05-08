README file for HW4. Describes how to COMPILE and RUN each program listed below.

-----------------------------------------------------------

-- helloWorld.f90 --

Compile: mpif90 -o helloWorld_mpi helloWorld_mpi.f90 

Run: mpirun -np 6 helloWorld_mpi

-----------------------------------------------------------

-- ssr_mpi.f90 --

Compile: mpif90 -o ssr_mpi ssr_mpi.f90

Run: mpirun -np

-----------------------------------------------------------

-- ping_pong_mpi.f90 --

Compile: mpif90 -o pong_pong_mpi ping_pong_mpi.f90

Run: mpirun -np 2 pong_pong_mpi

-----------------------------------------------------------

-- ring_mpi.f90 --

Compile: mpif90 -o ring_mpi ring_mpi.f90 

Run: mpirun -np 4 ring_mpi

-----------------------------------------------------------

-- pi_mpi.f90 --

Compile: mpif90 -o pi_mpi pi_mpi.f90

Run: mpirun -np 8 pi_mpi