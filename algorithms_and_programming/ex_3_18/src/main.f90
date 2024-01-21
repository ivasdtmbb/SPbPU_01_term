program exercise_3_18
   use Environment
   
   implicit none
   character(*), parameter    :: matrixA_file = "../data/matrixA.txt", matrixB_file = "../data/matrixB.txt", &
        output_file = "output.txt"
   integer                    :: In = 0, Out = 0, M, N, K, i
   character(:), allocatable  :: fmt
   real(R_), allocatable      :: matrixA(:, :), matrixB(:, :), referenceMatrix(:, :), &
        matrixC(:, :), matrixErr(:, :)

   ! Read the MatrixA(N, M), N - rows, M - columns
   open (file=matrixA_file, newunit=In)
      read (In, *) M, N
      allocate (matrixA(M, N))
      read (In, *) (matrixA(i, :), i = 1, M)
   close (In)

   ! Read the MatrixB(M, K), M - rows, K - columns
   open (file=matrixB_file, newunit=In)
      read (In, *) N, K
      allocate (matrixB(N, K))
      read (In, *) (matrixB(i, :), i = 1, N)
      allocate (referenceMatrix(M, K))
      allocate (matrixC(M, K))
      allocate (matrixErr(M, K))      
   close (In)

   ! Find a reference Matrix with matmul function
   referenceMatrix = matmul(matrixA, matrixB)

   ! Find the matrixes Multiplication with use of matrixMult function
   matrixC = matrixMult(matrixA, matrixB)

   ! Error check
   matrixErr = referenceMatrix - matrixC
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(/, "Matrix A:")')
      fmt = "(" //N// "f8.2)"
      write (out, fmt) (matrixA(i, :), i = 1, M)

      write (out, '(/, "Matrix B:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (matrixB(i, :), i = 1, N)

      write (out, '(/, "Refference Matrix:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (referenceMatrix(i, :), i = 1, M)

      write (out, '(/, "Matrix C:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (matrixC(i, :), i = 1, M)

      write (out, '(/, "Error Check:")')
      fmt = "(" //K// "f8.2)"
      write (out, fmt) (matrixErr(i, :), i = 1, M)
      
   close (Out)

 contains

   ! Чистая функция в императивном стиле.
   pure function MatrixMult(A, B) result(C)
      real(R_)    A(M, N), B(N, K), C(M, K)
      intent(in)  A, B
      integer     mC, nC, kC

!       do mC = 1, M
!          do kC = 1, K
!             C(mC, kC) = SUM(A(mC, :) * B(:, kC))
! !            C(mC, kC) = SUM([(A(mC, nC) * B(nC, kC), nC = 1, N)])
!          end do
!       end do

      ! I=M K=N J=K
      C = 0.0
      do kC = 1, K
         do nC = 1, N
            C(:, kC) = C(:, kC) + (A(:, nC) * B (nC, kC))      
          !   do iC = 1, I
          !      C(iC, jC) = C(iC, jC) + (A(iC, kC) * B (kC, jC))

            ! end do
         end do
      end do

      
      ! C = 0.0
      ! do iC = 1, I
      !    do jC = 1, J
      !       do kC = 1, K
      !          C(iC, jC) = C(iC, jC) + (A(iC, kC) * B (kC, jC))
      !       end do
      !    end do
      ! end do

   end function MatrixMult

end program exercise_3_18
