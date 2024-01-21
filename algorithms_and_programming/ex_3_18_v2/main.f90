program exercise_3_18_v2
   use Environment
   USE OMP_LIB
   
   implicit none
   character(*), parameter    :: matrixA_file = "../data/matrixA.txt", matrixB_file = "../data/matrixB.txt", &
        output_file = "output.txt"
   integer                    :: In = 0, Out = 0, M, N, K, i
   character(*), parameter :: E_ = "UTF-8"
   character(:), allocatable  :: fmt
   real(R_), allocatable      :: matrixA(:, :), matrixB(:, :), matrixC(:, :)

   ! Read the MatrixA(M, N), M - rows, N - columns
   open (file=matrixA_file, newunit=In)
      read (In, *) M, N
      allocate (matrixA(N, M)) ! matrixA(j, i): j - column number, i - row number
      read (In, *) matrixA
   close (In)

   ! Read the MatrixB(N, K), N - rows, K - columns
   open (file=matrixB_file, newunit=In)
      read (In, *) N, K
      allocate (matrixB(K, N)) ! matrixB(j, i): j - column number, i - row number
      read (In, *) matrixB
      allocate (matrixC(M, K))
   close (In)

   ! Find the matrixes Multiplication with use of matrixMult function
   matrixC = matrixMult(matrixA, matrixB)

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(/, "Matrix A:")')
      fmt = "(" //N// "f8.2)"
      write (out, fmt) (matrixA(1:N, :))

      write (out, '(/, "Matrix B:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (matrixB(1:K, :))

      write (out, '(/, "Matrix C:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (matrixC(i, :), i = 1, M)

   close (Out)

 contains

   ! Чистая функция в императивном стиле.
   pure function MatrixMult(A, B) result(C)
      real(R_)    A(N, M), B(K, N), C(M, K)
      intent(in)  A, B
      integer     i, j, t, jC, kC

      C = 0.0
      ! !$OMP PARALLEL DO
      ! do i = 1, M
      !    do j = 1, K
      !       C(i, j) = SUM([(A(t, i) * B(j, t), t = 1, N)])
      !    end do
      ! end do
      ! !$OMP END PARALLEL DO

      ! vectorization try later
      ! I K J          i - M j - K t - N
      !$OMP PARALLEL DO
      C = 0.0
         do jC = 1, K
            do kC = 1, N
              C(:, jC) = C(:, jC) + (A(:, kC) * B (kC, jC))      
            !   do iC = 1, I
            !    C(iC, jC) = C(iC, jC) + (A(iC, kC) * B (kC, jC))

            ! end do
         end do
      end do
      !$OMP END PARALLEL DO

   end function MatrixMult

end program exercise_3_18_v2
