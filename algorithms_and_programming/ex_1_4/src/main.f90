program exercise_1_4
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer(I_)                :: In = 0, Out = 0, i = 0, K = 3, t = 4
   real(R_), allocatable      :: Terms(:)
   real(R_)                   :: a = 0, denom = 0, const = 0.693147, ln_fa
   
   open (file=input_file, newunit=In)
      read (In, *) a
      allocate (Terms(K))
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "a", a
   close (Out)

   denom = t + a
   Terms(1) = a / denom

   do i = 2, 3
      Terms(i) = Terms(i-1) * (a**2) / (denom**2)
   end do
   
   ln_fa = const + 2 * Sum(Terms)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      fmt = "(a, T15, '= ', f10.4)"
      write (Out, fmt) "ln_f(2+a)", ln_fa
      ! Проверка:
      write (Out, fmt) "error", LOG(2 + a) - ln_fa
   close (Out)
end program exercise_1_4
