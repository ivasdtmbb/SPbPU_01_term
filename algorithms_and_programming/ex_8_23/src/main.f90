program exercise_8_23
  use Environment
  use Integral_IO
  use Integral_calculate

  implicit none
  character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
  real(R_)                :: p1 = 0, p2 = 0, dp = 0, q1 = 0, q2 = 0, dq = 0
  real(R_), allocatable   :: I(:, :), X(:), P(:), Q(:)
  integer                 :: pSize = 0, qSize = 0, iSize = 0

  call ReadP(input_file, p1, p2, dp, q1, q2, dq)
  call OutputPQ(output_file, p1, p2, dp, q1, q2, dq)

  pSize = Int((p2 - p1) / dp + .5_R_) + 1
  qSize = Int((q2 - q1) / dq + .5_R_) + 1
  iSize = pSize * qSize
  
  allocate(P(pSize), Q(qSize), X(N), I(pSize, qSize))
  
  call Integral(p1, dp, q1, dq, P, Q, X, I)

  call OutputIntegral(output_file, P, Q, I)

!  print "('iSize: ', i0)", iSize
end program exercise_8_23
  
