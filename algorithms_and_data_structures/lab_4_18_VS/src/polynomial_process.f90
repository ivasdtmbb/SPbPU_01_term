! Copyright_2023_Spodyneyko_V_Y
! 11-Dec-2023
!-------------------------------------------------------------------------------------------------!
module Polynomial_Process

   use Environment
   use Polynomial_IO
   
   implicit none

   integer, parameter      :: zero_exponent = 0

 contains

!_________________________Умножение_двух_полиномов_______________________________________________!
! 1. Все члены первого полинома последовательно умножаются на один из членов второго полинома.
!    Произведения добавляются к результирующему полиному (ResultPoly)
! 2. Процедура вызывается рекурсивно для следующего члена второго полинома.  
!------------------------------------------------------------------------------------------------!
   pure recursive subroutine Polynomials_Multi(FirstPoly, SecondPoly, ResultPoly)
     type(term), allocatable, intent(in)    :: FirstPoly, SecondPoly
     type(term), allocatable, intent(inout) :: ResultPoly

     if (Allocated(SecondPoly)) then
        call Polynomial_Factor_Multi(FirstPoly, SecondPoly, ResultPoly)
        call Polynomials_Multi(FirstPoly, SecondPoly%next, ResultPoly)
     end if
   end subroutine Polynomials_Multi

!_________________________Умножение_полинома_на_член__________________________________________!
!  Все члены полинома (Polynomial) последовательно умножаются на множитель (factor), новые члены
!  добавляются в начало результирующего полинома (ResultPoly)
!------------------------------------------------------------------------------------------------!
   pure recursive subroutine Polynomial_Factor_Multi(Polynomial, factor, ResultPoly)
     type(term), allocatable, intent(in)    :: Polynomial, factor
     type(term), allocatable, intent(inout) :: ResultPoly
     type(term), allocatable                :: newTerm

     if (Allocated(Polynomial)) then
        Allocate(newTerm)
        newTerm%coefficient = Polynomial%coefficient * factor%coefficient
        newTerm%exponent    = Polynomial%exponent + factor%exponent
!        resultPoly = term(Polynomial%coefficient * factor%coefficient, Polynomial%exponent + factor%exponent, resultPoly)

        call Move_Alloc(ResultPoly, newTerm%next)
        call Move_Alloc(newTerm, ResultPoly)

        call Polynomial_factor_Multi(Polynomial%next, factor, ResultPoly)
     end if
   end subroutine Polynomial_Factor_Multi

!_________________________Деление_двух_полиномов_________________________________________________!
! 1. Если старшая степень делимого (Dividend) больше или равна старшей степени делителя (Divider),
!   то создаём новый член частного (ResultPoly), разделив старшие члены делимого и делителя,
!   добавляем его в конец частного, чтобы потом не сортировать.
! 2. Находим новое делимое (NewDividend):
!     - умножаем делитель (Divider) на полученный член (ResultPoly%next)
!     - полученный полином вычитаем из делимого
! 3. Рекурсивно запускаем функцию с новым делимым (NewDividend), делителем (Dividend), ссылаемся на
!   последний полученный элемент частного (ResultPoly%next)
!------------------------------------------------------------------------------------------------!
   pure recursive subroutine Divide_Polynomials(Dividend, Divider, ResultPoly)
     type(term), allocatable, intent(in)    :: Dividend, Divider
     type(term), allocatable, intent(inout) :: ResultPoly
     type(term), allocatable                :: NewDividend, tmp

        if (Allocated(Dividend)) then
           if ((Dividend%exponent >= Divider%exponent)) then
              Allocate(NewDividend)
              NewDividend%coefficient = Dividend%coefficient / Divider%coefficient
              NewDividend%exponent    = Dividend%exponent - Divider%exponent

              call Move_Alloc(NewDividend, ResultPoly)

              call Polynomial_Factor_Multi(Divider, ResultPoly, NewDividend)
              call Polynomials_Subtraction(Dividend, NewDividend, tmp)
              call Move_Alloc(tmp, NewDividend)
              call Insertion_Sort_Terms(NewDividend, NewDividend)

              call Divide_Polynomials(NewDividend%next, Divider, ResultPoly%next)
           end if
        end if
   end subroutine Divide_Polynomials

!_________________________Вычитание_двух_полиномов_______________________________________________!
! 1. Инвертируем знаки коэффициентов вычитаемого (SecondPoly), присоединяем второй полином к 
!   первому. Складываем члены с одинаковыми степенями call Reduce_Polynomial
!------------------------------------------------------------------------------------------------!   
   pure subroutine Polynomials_Subtraction(FirstPoly, SecondPoly, ResultPoly)
     type(term), allocatable, intent(in) :: FirstPoly, SecondPoly
     type(term), allocatable, intent(inout) :: ResultPoly

     call Append_Polynomials(ResultPoly, FirstPoly)
     call Append_Polynomials(ResultPoly, Sign_Invert(SecondPoly))
     call Reduce_Polynomial(ResultPoly, ResultPoly%next)
   end subroutine  Polynomials_Subtraction

!________________Инверсия_знаков_коэффициентов_полинома__________________________________________!
   pure recursive function Sign_Invert(Polynomial) result(ResultPoly)
     type(term), allocatable, intent(in) :: Polynomial
     type(term), allocatable             :: ResultPoly

     Allocate(ResultPoly)
     if (Allocated(Polynomial)) then
        ResultPoly%coefficient = Polynomial%coefficient * (-1)
        ResultPoly%exponent = Polynomial%exponent
        ResultPoly%next = Sign_Invert(Polynomial%next)
     end if
   end function Sign_Invert

!_________________Добавление_элементов_второго_полинома_в_начало_первого_полинома________________!
   pure recursive subroutine Append_Polynomials(ResultPoly, termToAppend)
     type(term), allocatable, intent(in)    :: termToAppend
     type(term), allocatable, intent(inout) :: ResultPoly
     type(term), allocatable                :: tmp

     if (Allocated(termToAppend)) then
        Allocate(tmp)
        tmp%coefficient = termToAppend%coefficient
        tmp%exponent = termToAppend%exponent

        call Move_Alloc(ResultPoly, tmp%next)
        call Move_Alloc(tmp, ResultPoly)
        call Append_Polynomials(ResultPoly, termToAppend%next)
     end if
   end subroutine Append_Polynomials

!_________________________Сложение_членов_с_одинаковыми_степенями________________________________!
! Поочерёдно сравниваем наш текущий член с остальными, прибавляем коэффициенты членов с такой же
! степенью при неизвестной: a*x^n + b*x^n = (a+b)*x^n.
! Прибавленный член удаляется из структуры данных.
! Процедура выполняется рекурсивно и для сравнения членов и для выбора следующего члена.
!------------------------------------------------------------------------------------------------!
   pure recursive subroutine Reduce_Polynomial(termToReduce, currentTerm)
     type(term), allocatable, intent(inout)  :: termToReduce, currentTerm
     type(term), allocatable                 :: tmp

     if (Allocated(currentTerm)) then
        if (termToReduce%exponent == currentTerm%exponent) then
           termToReduce%coefficient = termToReduce%coefficient + currentTerm%coefficient
           call Move_Alloc(currentTerm, tmp)
           call Move_Alloc(tmp%next, currentTerm)

           call Reduce_Polynomial(termToReduce, currentTerm)
        else
           call Reduce_Polynomial(termToReduce, currentTerm%next)
        end if
     else if (Allocated(termToReduce%next)) then
        call Reduce_Polynomial(termToReduce%next, termToReduce%next%next)
     end if
   end subroutine Reduce_Polynomial

!_________________________Сортировка_вставками___________________________________________________!
   pure recursive subroutine Insertion_Sort_Terms(Polynomial, lastSorted)
      type(term), allocatable, intent(inout)  :: Polynomial, lastSorted
      type(term), allocatable                 :: tmp

      if (Allocated(lastSorted%next)) then
         if (lastSorted%next%exponent > lastSorted%exponent) then
            call Move_Alloc(lastSorted%next, tmp)
            call Move_Alloc(tmp%next, lastSorted%next)
            call Paste(Polynomial, tmp)
            call Insertion_Sort_Terms(Polynomial, lastSorted)
         else
            call Insertion_Sort_Terms(Polynomial, lastSorted%next)
         end if
      end if
    end subroutine Insertion_Sort_Terms

!________________Вставка_элемента_при_сортировке__________________________________________________!
    pure recursive subroutine Paste(current, itemToInsert)
      type(term), allocatable, intent(inout) :: current, itemToInsert
      type(term), allocatable                :: tmp
      
      if (itemToInsert%exponent > current%exponent) then
         call Move_Alloc(current, tmp)
         allocate (current, source=itemToInsert)
         call Move_Alloc(tmp, current%next)
      else
         call Paste(current%next, itemToInsert)
      end if
    end subroutine Paste

!________________Получение_остатка_от_деления_полинома_на_бином_по_схеме_Горнера__________________! 
!-------------------------------------------------------------------------------------------------!
! Polynomial - искомый полином; derivativeOrder - порядок производной для разложения по схеме Горнера;
! NewPolynomial - ссылка на первый член нового полинома (с максимальной степенью);
! a_zero_exp, b_zero_exp - нулевые показатели степени;
!-------------------------------------------------------------------------------------------------!
! 1. Получаем новый полином (NewPolynomial) путём разложения искомого (Polynomial) на бином (x-k)
!   по схеме Горнера. Перед запуском рекурсивной функции нахождения следующего члена (Gorner_Term)
!   размещаем нулевой член со старшей степенью, равной старшей степени исходного полинома.  
!   Kоэффициент при этом члене равен 0. Это делается для нахождения первого коэффициента в общем виде:
!   b_(n) = k * b_(n+1) + a_(n+1), n - степень члена, b_(n+1) = 0, =>
!   b_(n) = k * 0 + a_(n+1), =>
!   b_(n) = a_(n+1)
! 2. Удаляем первый член нового полинома, так как коэффициент равен 0
! 3. Пока порядок производной больше 0, продолжаем рекурсивно раскладывать полином по схеме Горнера 
! 4. Если порядок призводной не больше 0, находим остаток деления искомого полинома на бином (x-k):
!   remainder = k * b_(0) + a_(0), где a_(0) и b_(0) - коэффициенты при членах с нулевой степенью.    
!-------------------------------------------------------------------------------------------------!
    pure recursive function Calc_Gorner_Remainder(Polynomial, derivativeOrder, k) result(remainder)
      type(term), allocatable, intent(in)  :: Polynomial
      type(term), allocatable              :: NewPolynomial, tmp
      real(R_), intent(in)                 :: k
      real(R_)                             :: remainder, a_zero_exp, b_zero_exp
      integer, intent(in)                  :: derivativeOrder

      Allocate (NewPolynomial)
      NewPolynomial%exponent = Polynomial%exponent
      
      call Gorner_Term(Polynomial, NewPolynomial, k)
      call Move_Alloc(NewPolynomial, tmp)
      call Move_Alloc(tmp%next, NewPolynomial)

      if (derivativeOrder > 0) then
         remainder = Calc_Gorner_Remainder(NewPolynomial, derivativeOrder-1, k)
      else
         a_zero_exp = Get_Term_Coefficient(Polynomial, zero_exponent)
         b_zero_exp = Get_Term_Coefficient(NewPolynomial, zero_exponent)
         remainder = k * b_zero_exp + a_zero_exp
      end if
    end function Calc_Gorner_Remainder

!______________Получение_нового_члена_полинома_при_разложении_по_схеме_Горнера____________________!
!-------------------------------------------------------------------------------------------------!
! DividendPolynomial - искомый полином; k - второй моном бинома (x-k);
! currentTerm - последний член полинома, полученного при разложении искомого по схеме Горнера;
! nextTerm - новый член полинома, полученного при разложении искомого по схеме Горнера;
!-------------------------------------------------------------------------------------------------!
! Раскладываем искомый многочлен по схеме Горнера F(x)/(x-k), где k - аргумент функции, значение x:
!  1. Размещаем следующий член (nextTerm), его степень (n) на 1 меньше степени последнего члена (currentTerm)
!  2. В искомом полиноме находим коэффициент при члене со степенью (n+1) - dividendCoefficient 
!  3. Находим коэффициент для нового члена по формуле b_(n) = k * b_(n+1) + a_(n+1)
!  4. Добавляем новый член (nextTerm) после переданного последнего члена (currentTerm)
!  5. Рекурсивно повторяем операцию нахождения нового члена (Gorner_Term) до тех пор,
!     пока степень члена не станет равна 0.
!-------------------------------------------------------------------------------------------------!
    pure recursive subroutine Gorner_Term(DividendPolynomial, currentTerm, k)
      type(term), allocatable, intent(in)    :: DividendPolynomial
      type(term), allocatable, intent(inout) :: currentTerm
      type(term), allocatable                :: nextTerm
      real(R_), intent(in)                   :: k
      real(R_)                               :: dividendCoefficient
      
      Allocate(nextTerm)
      nextTerm%exponent = currentTerm%exponent - 1

      dividendCoefficient = Get_Term_Coefficient(DividendPolynomial, nextTerm%exponent + 1)
      nextTerm%coefficient = k * currentTerm%coefficient + dividendCoefficient
      call Move_Alloc(nextTerm, currentTerm%next)

      if (currentTerm%next%exponent > 0) then
         call Gorner_Term(DividendPolynomial, currentTerm%next, k)
      end if
    end subroutine Gorner_Term

!____________Получение_первой_производной_полинома________________________________________________!
! f(x) = x^n, f'(x) = n*x^(n-1)
!-------------------------------------------------------------------------------------------------!
    pure recursive function Differentiate(theTerm) result(difOfTerm)
      type(term), allocatable, intent(in) :: theTerm
      type(term), allocatable             :: difOfTerm

      Allocate(difOfTerm)
      if (Allocated(theTerm)) then
         if (theTerm%exponent > 0) then
            difOfTerm%coefficient = theTerm%coefficient * theTerm%exponent
            difOfTerm%exponent = theTerm%exponent - 1
            difOfTerm%next = Differentiate(theTerm%next)
         end if
      end if
    end function Differentiate
    
!____________Нахождение_значения_члена_полинома_при_известной_переменной_x=c______________________!
! F_n(c) = a * c^n 
!-------------------------------------------------------------------------------------------------!
    pure recursive subroutine Calculate_The_Term(theTerm, x, sum)
      type(term), allocatable, intent(in) :: theTerm
      real(R_), intent(in)                :: x
      real(R_), intent(inout)             :: sum

      if (Allocated(theTerm)) then
         sum = sum + theTerm%coefficient * (x ** theTerm%exponent)
         call Calculate_The_Term(theTerm%next, x, sum)
      end if
    end subroutine Calculate_The_Term

!____________Поиск_значения_коэффициента_при_члене_полинома_при_заданной_степени__________________!
! Члены полинома должны быть отсортированы по убыванию для правильного выполнения первого условного оператора. 
!-------------------------------------------------------------------------------------------------!
    pure recursive function Get_Term_Coefficient(currentTerm, exp) result(termCoefficient)
      type(term), allocatable, intent(in) :: currentTerm
      integer, intent(in)                 :: exp
      real(R_)                            :: termCoefficient

      if (Allocated(currentTerm) .and. (currentTerm%exponent >= exp)) then
         if (currentTerm%exponent == exp) then
            termCoefficient = currentTerm%coefficient
         else
            termCoefficient = Get_Term_Coefficient(currentTerm%next, exp)
         end if
      else
         termCoefficient = 0
      end if
    end function Get_Term_Coefficient
!-------------------------------------------------------------------------------------------------!
  end module Polynomial_Process
