program exercise_8_23
   use Environment
   use Integral_IO
   use Integral_calculate 

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_)                :: p1 = 0, p2 = 0, delta_p = 0
   real(R_), allocatable   :: I(:), P(:), Q(:), X(:)
   integer                 :: pSize, qSize

   call ReadPQ(input_file, p1, p2, delta_p, q1, q2, delta_q)
   
   call OutputP(output_file, p1, p2, delta_p, q1, q2, delta_1)

   pSize = Int((p2 - p1) / delta_p + .5_R_) + 1
   qSize = Int((q2 - q1) / delta_q + .5_R_) + 1
   
   allocate(P(pSize), Q(qSize), X(N), I(M))

   call Integral(p1, delta_p, P, X, I)
   
   call OutputIntegral(output_file, P, I)
end program exercise_8_23
