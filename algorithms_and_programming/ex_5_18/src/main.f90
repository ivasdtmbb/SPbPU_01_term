program exercise_5_18
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, &
        Size = 0, Quantity = 0, Min = 0, Max = 0
   integer                 :: Counter = 0
   integer, allocatable    :: Elements(:), ElementsInRange(:)
   logical, allocatable    :: InRange(:)
   real(R_)                :: MeanValue = 0.0

   open (file=input_file, newunit=In)
      read (In, *) Size, Quantity, Min, Max
      allocate (Elements(Size))
      read (In, *) Elements
   close (In)

   allocate(InRange(Size))
   allocate(ElementsInRange(Quantity))

   call CountMeanValue(Elements, InRange, Quantity, Min, Max, Counter, MeanValue, ElementsInRange)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4i6)") Size, Quantity, Min, Max
      write (Out, "("//Size//"(i4))") Elements
      write (Out, '(/a, T20, "= ", i0)') 'Elements in range', Counter
      write (Out, '(a, T10, "= ", f10.2)') "Mean", MeanValue

      write (Out, "(i0, 1x)", advance="no") ElementsInRange(1:Counter)
!      write (Out, "("//Size//"(L2))") InRange
   close (Out)

contains
  
   pure subroutine CountMeanValue(Elements, InRange, Quantity, Min, Max, Counter, MeanValue, ElementsInRange)
      integer     Elements(:), Quantity, Min, Max, Counter
      integer, allocatable     ::    ElementsInRange(:)
      logical, allocatable      ::     InRange(:)
      real        MeanValue
      intent(in)  Elements, Quantity, Min, Max
      intent(out) InRange, MeanValue, Counter, ElementsInRange

      InRange = Elements > Min .and. Elements < Max
      ElementsInRange = PACK(Elements, InRange)
      
      Counter = MINVAL([Count(InRange), Quantity])
      MeanValue = Sum(ElementsInRange(1:Counter)) / Real(Counter)
      
   end subroutine CountMeanValue
end program exercise_5_18
