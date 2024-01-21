program exercise_6_2g
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: sh_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
!   sh_x = SinHXImp(x)
   sh_x = SinHX(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Sh(x)", sh_x, "Fortran SinH(x)", SINH(x), "Error", sh_x - SINH(x)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   real(R_)  function SinHXImp(x) result(SinHX)
      real(R_), intent(in) :: x
      
      ! 2pi == 2 * 4*Arctg(1), т. к. Tg(1) = pi/4 => pi == 4*Arctg(1)
      real(R_)    r, q, x_s, x_2, OldSinHX
      integer     n

      x_s = x

      x_2   = x_s * x_s
      
      n     = 0
      r     = x_s
      SinHX  = r

      ! Цикл с постусловием: пока сумма не перестанет меняться.
      do
         n        = n + 2
         q        = - x_2 / (n*(n + 1))
         r        = r * q
         OldSinHX  = SinHX
         SinHX     = SinHX + r
         if (OldSinHX == SinHX) exit
      end do
      print "('Число членов суммы: ', i0)", n / 2 + 1
      print "('Число итераций: ', i0)", n / 2
      print "('OldSinHX:  ' , f40.30)", OldSinHX
      print "('SinHX:     ' , f40.30)", SinHX
      print "('SinHX + r: ' , f40.30)", SinHX + r 
      print "('q:        ' , f40.30)", q
      print "('r:        ' , f40.30)", r
      print "('r * q:    ' , f40.30)", r * q

   end function SinHXImp


   ! yanked "pure"
   real(16) function SinHX(x)
      real(R_), intent(in) :: x
      
      real(R_) R(4), Numerators(4), Denominators(4), n_fact, x_s, x_8
      integer  Ns(8)

      x_s = x
    
      Numerators = x_s ** [3, 5, 7, 9]
      x_8 = Numerators(4) / x_s
      Denominators = [2*3, 2*3*4*5, 2*3*4*5*6*7, 2*3*4*5*6*7*8*9]
      Ns = [2, 4, 6, 8, 3, 5, 7, 9]

      ! Вычисление суммы членов со 2-ого по 5-ый.
      R = Numerators / Denominators

      SinHX = x_s + Sum(R)
      
      do while (SinHX + R(4) /= SinHX)

         Numerators = Numerators * x_8
         ! Вычисление очередных знаменателей-факториалов: (n+2)!, (n+4)!, (n+6)!, (n+8)!
         n_fact = Denominators(4)
      
         Ns = Ns + 8
         
         Denominators = Ns(1:4) * Ns(5:8)

         ! Вычисление знаменателей: (n+2)!, (n+4)!, (n+6)!, (n+8)!
         Denominators(1) = n_fact * Denominators(1)
         Denominators(2) = Denominators(1) * Denominators(2) ! == (n+4)! == (n+2)! * (n+3)*(n+4)
         Denominators(3) = Denominators(2) * Denominators(3) ! == (n+6)! == (n+4)! * (n+5)*(n+6)
         Denominators(4) = Denominators(3) * Denominators(4) ! == (n+8)! == (n+6)! * (n+7)*(n+8)
         
         R = Numerators / Denominators

         SinHX = SinHX + Sum(R)
      end do
      print "('Число членов суммы: ', i0)", (Ns(8)+1) / 2
      print "('Число итераций: ', i0)", (Ns(8)-1) / 8
   end function SinHX 
end program exercise_6_2g
