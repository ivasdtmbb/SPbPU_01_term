program exercise_4_6g
   use Environment
   
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0
   real(R_)                :: a = 0, b = 0, h = 0, I = 0
   real(R_), allocatable   :: X(:)

   open (file=input_file, newunit=In)
      read (In, *) a, b, h
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", h
   close (Out)
  
   N = Int((b - a) / h + .5_R_) + 1

   allocate(X(N))
   call Integrate(a, h, X, I)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(" // N // "f6.2)") X
      write (Out, '(a, T4, "= ", f0.4)') "I", I
   close (Out)

contains

  ! Чистая подпрограмма в регулярном стиле.
   pure subroutine Integrate(a, h, X, I)
      real(R_)    a, h, X(:), I
      intent(in)  a, h
      intent(out) X, I
      integer     j
       
      X = [(a + j*h, j = 0, Size(X) - 1)]
      X = .365_R_ * Exp(-X) / (2 * COS(X) + 3)
      X(1) = X(1) / 2    
      X(N) = X(N) / 2    
      I = Sum(X) * h
      ! I = X(1)/2 + Sum(X(2:N-1)) + X(N)/2    ! vectorisation wouldn't work, bad practice
   end subroutine Integrate
end program exercise_4_6g
