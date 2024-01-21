program exercise_3_18_v2
   !use Environment

   
   implicit none
   character(*), parameter    :: matrixA_file = "matrixA.txt", matrixB_file = "matrixB.txt", output_file = "output.txt"
   integer, parameter         :: R_ = 8
   integer                    :: In = 0, M, N, K
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

 contains

    pure function MatrixMult(A, B) result(C)
      real(8)    A(N, M), B(K, N), C(M, K)
      intent(in)  A, B
      integer     jC, kC ! timer

!       do timer = 1, 20
         C =  0.0
            do jC = 1, K
               do kC = 1, N
               C(:, jC) = C(:, jC) + (A(:, kC) * B (kC, jC))
            end do
         end do
!       end do

   end function MatrixMult

end program exercise_3_18_v2
