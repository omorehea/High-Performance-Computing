README file for Game of Life Project. Describes how to COMPILE and RUN each program listed below.


-- All program versions utilize the module, gol_subs.f90 --

-- All program versions have their own Makefile which can be
   used to make the executables (already provided) -- 

-- Upon running all program versions, the user will be prompted
   to input the desired initial grid formation, as well as
   the dimension of the square grid --

-- The visualized grid data will be printed to the screen --


-- On grape, do the following to compile and run my code interactively --
-----------------------------------------------------------

-- 1D Column-Wise Data Decomposition --

### gol_mpi_columns.f90 --> executable: gol_col ###

Compile W/ Makefile: make gol_col
 
Compile W/out Makefile: mpif90 -o gol_col gol_subs.f90 gol_mpi_columns.f90

Run: mpirun -np 4 gol_col  (can edit the number of procs used here)

-----------------------------------------------------------

-- 1D Row-Wise Data Decomposition --

### gol_mpi_rows.f90 --> executable: gol_row ###

Compile W/ Makefile: make gol_row

Run: mpirun -np 4 gol_row  (can edit the number of procs used here)

-----------------------------------------------------------

-- 2D Data Decomposition --

### gol_mpi_2d.f90 --> executable: gol_2d ###

Compile W/ Makefile: make gol_2d

Run: mpirun -np 4 gol_2d  (currently, only 4 procs works here!)

-----------------------------------------------------------

-- Serial Version --

### gol_serial.f90 --> executable: gol_serial ###

Compile W/ Makefile: make gol_serial

Run: ./gol_serial

------------------------------------------------------------