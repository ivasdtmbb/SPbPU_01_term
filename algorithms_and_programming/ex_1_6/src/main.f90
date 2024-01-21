

program exercise_1_6
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, i = 0, k = 0
   real(R_)                   :: x = 0, xexp2 = 0, temp = 0, tg_x = 0
   
   open (file=input_file, newunit=In)
      read (In, *) x
      read (In, *) k
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f25.15)"
      write (Out, fmt) "x", x
      write (Out, "(a, T7, '=', i4)") "k", k
   close (Out)

   xexp2 = x**2
   temp = real(k, R_)
   
   do i = 2, 4
      k = k - 2
      temp = k - (xexp2 / temp)
   end do
      
   tg_x = x / temp

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, fmt) "tg_x", tg_x
      ! Проверка:
      write (Out, fmt) "error", tan(x) - tg_x
   close (Out)
end program exercise_1_6
