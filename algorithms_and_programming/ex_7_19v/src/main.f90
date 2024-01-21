program exercise_7_19v
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, N_zero
   real(R_), allocatable   :: C(:, :)
!   real(R_)                :: max_neg = 0, min_pos = 0
   integer, allocatable    :: Indexes(:, :), Ind_zero(:, :)
   logical, allocatable    :: Mask(:)

   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)

   allocate (Indexes(N*M, 2))
   allocate (Mask(N*M), source=.false.)
  
   call zeroElementsPos(C, Mask, Indexes, Ind_zero, N_zero)
!   N_zero = Count( C == 0 )
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, i3)') "Количество элементов матрицы, равных нулю:", N_zero
      write (Out, '(2i3)') (Ind_zero(i, :), i = 1, UBound(Ind_zero, 1))
   close (Out)

contains

   pure subroutine zeroElementsPos(C, Mask, Indexes, Ind_zero, N_zero)
      real(R_), intent(in)               :: C(:, :)
      integer,  intent(out)              :: Indexes(:, :), N_zero
      integer,  allocatable, intent(out) :: Ind_zero(:, :)
      logical,  intent(out)              :: Mask(:)
      integer   i, j

      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      ! Получаем маску для элементов, равных нулю:
      Mask        = [C == 0]
      N_zero   = Count(Mask)
      
      ! Размещение массивов индексов.
      allocate(Ind_zero(N_zero, 2))
      ! Упаковка массива индексов по каждой из координат.
      Ind_zero(:, 1) = Pack(Indexes(:, 1), Mask)
      Ind_zero(:, 2) = Pack(Indexes(:, 2), Mask)
 
   end subroutine zeroElementsPos
end program exercise_7_19v
