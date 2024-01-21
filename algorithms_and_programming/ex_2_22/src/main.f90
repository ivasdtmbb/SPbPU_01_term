program exercise_2_22
   use Environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0
   real(R_)                   :: x = 0, a = 0, b = 0, c = 0, delta = 0, fval = 0

   open (file=input_file, newunit=In)
      read (In, *) x, a, b, c
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(4(a, f10.4/))"
      write (Out, fmt) "x =", x, "a =", a, "b =", b, "c =", c
   close (Out)

   delta = (4 * a * c) - b**2
   fval = U(x, a, b, c, delta)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      fmt = "(a, f10.4)"
      write (Out, fmt) "delta =", delta
      write (Out, fmt) "f =", fval
   close (Out)

contains

  ! Чистая функция.
   pure function U(x, a, b, c, delta)
      real(R_) U, x, a, b, c, delta, temp, unit1, unit2
      intent(in)  x, a, b, c, delta

      unit1 = 1 / SQRT(a)
      unit2 = 2*a + b*x

      if (a > 0 .and. delta /= 0) then
         temp = (unit2 + 2*SQRT(a * (a + b*x + c*(x**2)))) / x
         U = -1 * unit1 * LOG(ABS(temp))
      else if (a < 0 .and. delta < 0) then
         temp = unit2 / (ABS(x) * SQRT(-1 * delta))
         U = unit1 * ASIN(temp)
      else if (a > 0 .and. delta == 0) then
         temp = x / (2*a + b*x)
         U = unit1 * LOG(temp)
      else
         ! Присвоение не числа -- NaN.
         U = IEEE_Value(x, IEEE_Quiet_NaN)
      end if
   end function U
      
end program exercise_2_22
