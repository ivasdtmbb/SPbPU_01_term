program exercise_7_2b
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0
   real(R_), allocatable   :: Array(:), Positives(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) M
      allocate (Array(M))
      read (In, *) Array
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f6.2)") Array
   close (Out)
 

   ! Формируем маску для положительных эелементов.
   allocate(Pos(M), source = .false.)
   allocate(Positives(M))

   Pos = Array > 0
   N = Count(.Not.Pos)
   Array = [PACK(Array, .Not. Pos), PACK(Array, Pos)]

   call SortDescending(Array(N+1:))
  
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//M//"L2)") Pos
      write (Out, "(/"//M//"f7.2)") Array
   close (Out)

contains

   pure subroutine SortDescending(PosNums)
     real(R_), intent(inout)  :: PosNums(:)
     real(R_)                 :: tmp
     integer                  :: i, MaxInd

     do i = 1, Size(PosNums) - 1
        MaxInd = MaxLoc(PosNums(i:), 1) + i-1
        if (i /= MaxInd) then
           tmp             = PosNums(i)
           PosNums(i)      = PosNums(MaxInd)
           PosNums(MaxInd) = tmp
        end if
     end do
   end subroutine SortDescending


   !   do concurrent (i = 1:(Size(PosNums) - 1), i /= MaxLoc(PosNums(i:), 1) + i-1)
   !         tmp             = PosNums(i)
   !         PosNums(i)      = PosNums(MaxInd)
   !         PosNums(MaxInd) = tmp
   !   end do
   ! end subroutine SortDescending


 end program exercise_7_2b


   ! real(R_)                 :: tmp
   !   do i = 1, Size(PosNums) - 1
   !      MaxInd = MaxLoc(PosNums(i:), 1) + i-1
   !      if (i /= MaxInd) then
   !         tmp             = PosNums(i)
   !         PosNums(i)      = PosNums(MaxInd)
   !         PosNums(MaxInd) = tmp
   !      end if
   !   end do
   ! end subroutine SortDescending
