program exercise_7_16d
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:, :), Sums(:)
   real(R_)                :: lastElement

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (Z(i, :), i = 1, N)
   close (Out)

   allocate(Sums(N), source=0._R_)
   call UpperSum(Z, Sums, lastElement)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//N//"f7.2)") Sums
      write  (Out, '(a, T55, "= ", f7.2)') "Наибольший элемент столбца", lastElement
   close (Out)

contains
  
  pure subroutine UpperSum(Z, Sums, lastElement)
      real(R_)       Z(:, :), Sums(:), minTab(N)
      real(R_)       lastElement
      intent(in)     Z
      intent(inout)  Sums, lastElement
      integer        j, minTabLocation
 
      ! do concurrent (j = 1:N)
      !    Sums(j) = Sum(Z(:, j)) ! dim
      ! end do
      ! minTabLocation = MINLOC(Sums, 1)
      ! minTab = Z(:, minTabLocation)
      ! lastElement = MaxVal(minTab)
      ! lastElement = MaxVal(Z(:, MinLoc(Sum(Z(:, j), j = 1, N), 1)))
      ! lastElement = MaxVal(Z(:, MinLoc(Sums, 1)))

      Sums = Sum(Z, dim=1)
      lastElement = MaxVal(Z(:, MinLoc(Sums, dim=1)))

   end subroutine UpperSum
end program exercise_7_16d
