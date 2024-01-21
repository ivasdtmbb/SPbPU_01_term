program randomMatrix
   use Environment
   
   implicit none
   character(*), parameter    :: matrixA_file = "../data/matrixA.txt", &
        output_file = "output.txt"
   integer                    :: In = 0, Out = 0, M, N, f
   character(:), allocatable  :: fmt
   real(R_), allocatable      :: arrayA(:), matrixA(:, :)
   integer(I_), allocatable    :: intArray(:)

!   Read the MatrixA size N - rows, M - columns
   open (file=matrixA_file, newunit=In)
      read (In, *) N, M
      allocate (matrixA(N, M))
   close (In)

   ! open (file=matrixA_file, newunit=In)
   !    read (In, *) N
   !    allocate (arrayA(N))
   !    allocate (intArray(N))
   ! close (In)


   ! Fill up the matrix with random real numbers
   call random_number(matrixA)

   ! ! Fill up the array with integers
   ! call random_number(arrayA)
   ! intArray = int(arrayA * 100) 

   ! ! array of integers
   ! open (file=output_file, encoding=E_, newunit=Out)   
   !    fmt = "(" // N // "i4)"  
   !    write (Out, fmt) intArray
   ! close (Out)

   ! matrix of real
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(2I6)") N, M
      fmt = "(" //M// "f8.2)"
      write (Out, fmt) (matrixA(f, :), f = 1, N)
   close (Out)

 contains

end program randomMatrix
