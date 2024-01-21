program exercise_7_38
   use Environment
   
   implicit none
   character(*), parameter    :: arrayA_file = "../data/matrixA.txt", arrayB_file = "../data/matrixB.txt", &
        output_file = "output.txt"
   integer                    :: In = 0, Out = 0, M, N, K, i
   character(:), allocatable  :: fmt
   integer, allocatable       :: A(:, :), B(:, :), matrixC(:, :)

   ! Read the A(M, N), M - rows, N - columns
   open (file=arrayA_file, newunit=In)
      read (In, *) M
      allocate (A(M, 1))
      read (In, *) A
   close (In)

   ! Read the B(N, K), N - rows, K - columns
   open (file=arrayB_file, newunit=In)
      read (In, *) N
      allocate (B(1, N))
      read (In, *) B
      allocate (matrixC(M, N))
   close (In)

   ! Find the matrixes Multiplication with use of matrixMult function
   matrixC = MatMul(A, B)

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(/, "Array A:")')
      fmt = "(" //M// "i5)"
      write (out, fmt) A(:, 1)

      write (out, '(/, "Array B:")')
      fmt = "(" //N// "i5)"
      write (out, fmt) B(1, :)

      write (out, '(/, "Matrix C:")')
      fmt = "(" //N// "i6)"
      write (out, fmt) (matrixC(i, :), i = 1, M)
   close (Out)

end program exercise_7_38
