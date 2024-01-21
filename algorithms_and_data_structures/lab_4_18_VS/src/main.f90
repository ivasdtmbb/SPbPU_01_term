! Copyright_2023_Spodyneyko_V_Y
! 11-Dec-2023
!_______________________Задание________________________________________________________!
! Два полинома
! P_n(x) = a_(n)*x^n + a_(n-1)*x^(n −1) + .... + a_0
! Q_m(x) = b_(m)*x^m + b_(m-1)*x^(m −1) + .... + b_0
! заданы в файле в виде пар “коэффициент-показатель степени“:
! (a n) , (a n-1) ...(a,0)
! (b m ), (b m-1) ...(b,0)
! * Используя линейные списки, составить процедуру формирования произведения полиномов
!   F(X)=P_n(x)*Q_m(x), вычислить производную этой функции, используя схему Горнера
! * Используя линейные списки, составить процедуру формирования первой
!   производной от полинома F(x)=P_n(x)/Q_m(x)
!_______________________Пояснение________________________________________________________!
! * Для вычисления производной используем схему Горнера:
!   F^k(x) = k! * b_(n-k), где:
!   F^k(x) - производная порядка k от функции F(x) -> (derivativeF_x)
!   k! - факториал порядка производной -> (factorial)
!   b_(n-k) - остаток, полученный при разложении полинома по схеме Горнера k раз -> (gornerRemainder)
! * Деление полиномов производим в столбик, остаток отбрасывается, производную считаем
!   обычным способом
!---------------------------------------------------------------------------------------!

program lab_4_18_VS
   use Environment
   use Polynomial_Process
   use Polynomial_IO

   implicit none
   character(:), allocatable  :: first_poly_file, second_poly_file, x_file, output_file
  
   type(term), allocatable    :: FirstPoly, SecondPoly, ResultPoly
   real(R_)                   :: x, gornerRemainder, derivativeF_x
   integer                    :: derivativeOrder, factorial, i

   first_poly_file  = "../data/polynomialA.txt"
   second_poly_file = "../data/polynomialB.txt"
   x_file           = "../data/x_value.txt"
   output_file      = "output.txt"
 
   FirstPoly  = Read_Polynomial(first_poly_file)
   SecondPoly = Read_Polynomial(second_poly_file)
   call Read_X(x_file, x, derivativeOrder)

   call Output_Terms_List(output_file, FirstPoly, "Первый полином:", "rewind")
   call Output_Terms_List(output_file, SecondPoly, "Второй полином:", "append")
   call Output_Value(output_file, x, "Значение x:", "append")

   if (Allocated(FirstPoly) .and. Allocated(SecondPoly)) then

      Allocate(ResultPoly)
      call Polynomials_Multi(FirstPoly, SecondPoly, ResultPoly)

      call Output_Terms_List(output_file, ResultPoly, "Результат умножения полиномов:", "append")
      call Reduce_Polynomial(ResultPoly, ResultPoly%next)
      call Insertion_Sort_Terms(ResultPoly, ResultPoly)
      call Output_Terms_List(output_file, ResultPoly, "После сжатия и сортировки:", "append")

      GornerRemainder = Calc_Gorner_Remainder(ResultPoly, DerivativeOrder, x)
      factorial = Product((/(i, i=1, derivativeOrder)/))
      derivativeF_x = GornerRemainder * factorial
      call Output_Value(output_file, derivativeF_x, "F(x)=P_n(x)*Q_m(x), F'(x):", "append")

      Deallocate(ResultPoly)

      Allocate(ResultPoly)
      
      call Divide_Polynomials(FirstPoly, SecondPoly, ResultPoly)
      call Insertion_Sort_Terms(ResultPoly, ResultPoly)
      call Output_Terms_List(output_file, ResultPoly, "Частное:", "append")

      GornerRemainder = Calc_Gorner_Remainder(ResultPoly, 0, x)
      factorial = 1
      derivativeF_x = GornerRemainder * factorial
      call Output_Value(output_file, derivativeF_x, "F(x)=P_n(x)/Q_m(x), Горнер, F(x):", "append")

      GornerRemainder = Calc_Gorner_Remainder(ResultPoly, derivativeOrder, x)
      factorial = Product((/(i, i=1, derivativeOrder)/))
      derivativeF_x = GornerRemainder * factorial
      call Output_Value(output_file, derivativeF_x, "F(x)=P_n(x)/Q_m(x), Горнер, F'(x):", "append")

      derivativeF_x = 0
      call Calculate_The_Term(ResultPoly, x, derivativeF_x)
      call Output_Value(output_file, derivativeF_x, "F(x)=P_n(x)/Q_m(x), Простой способ, F(x):", "append")
      ResultPoly = Differentiate(ResultPoly)
      call Output_Terms_List(output_file, ResultPoly, "Производная:", "append")
      derivativeF_x = 0
      call Calculate_The_Term(ResultPoly, x, derivativeF_x)
      call Output_Value(output_file, derivativeF_x, "F(x)=P_n(x)/Q_m(x), Простой способ, F'(x):", "append")
   end if
!-----------------------------------------------------------------------------------------!
 end program lab_4_18_VS
