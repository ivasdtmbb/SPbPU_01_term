module Integral_IO
   use Environment

   implicit none
 contains
   ! Чтение параметра p.
   subroutine ReadP(input_file, p1, p2, dp, q1, q2, dq)
      character(*), intent(in) :: input_file
      real(R_), intent(out)    :: p1, p2, dp, q1, q2, dq

      integer :: In = 0
   
      open (file=input_file, newunit=In)
         read (In, *) p1, p2, dp
         read (In, *) q1, q2, dq
      close (In)
   end subroutine ReadP
  
   ! Вывод параметров p и q.
   subroutine OutputPQ(output_file, p1, p2, dp, q1, q2, dq)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: p1, p2, dp, q1, q2, dq

      integer :: Out = 0
   
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(3(a, T7, "= ", f0.4/))') "p1", p1, "p2", p2, "dp", dp
         write (Out, '(3(a, T7, "= ", f0.4/))') "q1", q1, "q2", q2, "dq", dq
      close (Out)
    end subroutine OutputPQ
   
    subroutine OutputIntegral(output_file, P, Q, I)
      character(*), intent(in) :: output_file
      character(:), allocatable   :: fmt
      real(R_), intent(in)     :: I(:, :), P(:), Q(:)
      integer :: M
      integer :: Out = 0, k = 0
      
      open (file=output_file, encoding=E_, newunit=Out, position='append')
         M = Size(Q)
         fmt = "("//M//"f6.2)"
         write (Out, fmt) Q

         M = M + 1
         fmt = "("//M//"f6.2)"
         write (Out, fmt) (I(k, :), P(K), k = 1, Size(P))

      close (Out)
    end subroutine OutputIntegral
    
end module Integral_IO
