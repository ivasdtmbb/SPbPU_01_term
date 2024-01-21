program exercise_1_6_r
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, I = 1, deepness = 0
   real(R_)                   :: x = 0, xexp2 = 0, tg_x = 0
   
   open (file=input_file, newunit=In)
      read (In, *) x
      read (In, *) deepness
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f25.15)"
      write (Out, fmt) "x", x
      write (Out, "(a, T10, '=', i4)") "deepness", deepness
   close (Out)

   xexp2 = x**2
   tg_x = x / deepdenom(xexp2, deepness, i)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append'?)
      write (Out, fmt) "tg_x", tg_x
      ! Проверка:
      write (Out, fmt) "error", tan(x) - tg_x
   close (Out)

contains

   recursive real(R_) function deepdenom(xexp2, deepness, coef) result(denom)
      integer(I_)  :: deepness, coef
      real(R_)     :: xexp2
      intent(in)   :: xexp2, deepness, coef

      if (deepness <= coef) then
         denom = real(coef, R_)
      else
         denom = real(coef, R_) - xexp2 / deepdenom(xexp2, deepness, coef+2)
      end if
   end function deepdenom
 
end program exercise_1_6_r
