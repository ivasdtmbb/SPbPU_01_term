
program exercise_7_30
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: Matrix(:, :), AbsMatrix(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (Matrix(M, N)) ! Хранение по строкам: Matrix(j, i), где j-номер столбца, i-номер строки
      allocate (AbsMatrix(M, N))
      read (In, *) (Matrix(:, i), i = 1, N)
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (Matrix(:, i), i  = 1, N)
   close (Out)

   AbsMatrix = ABS(Matrix)
   call sortColumns(Matrix, AbsMatrix, N, M)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a)') "--------------------------"
      write (Out, '('//M//'f6.2)') (Matrix(:, i), i  = 1, N)
   close (Out)

contains

  pure subroutine sortColumns(Matrix, AbsMatrix, N, M)
    integer,  intent(inout) :: N, M
    real(R_), intent(inout) :: Matrix(:, :), AbsMatrix(:, :)
!    real(R_), allocatable   :: AbsMatrix(:, :) ! AbsColumn(:)
    real(R_), allocatable   :: AbsRow(:)
    real(R_)                :: tmp, tmpAbs
    integer                 :: row, i, minIndex

    ! do concurrent (column = 1:N, i = 1:M)
    !    minIndex = MinLoc(AbsMatrix(i:, column), 1) + i-1
    !    if (i /= minIndex) then
    !       tmpAbs                      = AbsMatrix(i, column)
    !       AbsMatrix(i, column)        = AbsMatrix(minIndex, column)
    !       AbsMatrix(minIndex, column) = tmpAbs

    !       tmp                         = Matrix(i, column)
    !       Matrix(i, column)           = Matrix(minIndex, column)
    !       Matrix(minIndex, column)    = tmp
    !    end if
    ! end do

    ! Нет строк и столбцов, есть первый, второй и.т.д. индексы
    ! do row = 1, N
    !    AbsRow = ABS(Matrix(:, row))
    !    do i = 1, Size(AbsRow) - 1
    !       minIndex = MinLoc(AbsRow(i:), 1) + i-1
    !       if (i /= minIndex) then
    !          tmpAbs                   = AbsRow(i)
    !          AbsRow(i)                = AbsRow(minIndex)
    !          AbsRow(minIndex)         = tmpAbs

    !          tmp                      = Matrix(i, row)
    !          Matrix(i, row)           = Matrix(minIndex, row)
    !          Matrix(minIndex, row)    = tmp
    !       end if
    !    end do
    ! end do

    do row = 1, N
       AbsRow = ABS(Matrix(:, row))
       do i = 1, Size(AbsRow) - 1
          minIndex = MinLoc(AbsRow(i:), 1) + i-1
          if (i /= minIndex) then

             tmpAbs                   = AbsRow(i)
             AbsRow(i)                = AbsRow(minIndex)
             AbsRow(minIndex)         = tmpAbs

             tmp                      = Matrix(i, row)
             Matrix(i, row)           = Matrix(minIndex, row)
             Matrix(minIndex, row)    = tmp
          end if
       end do
    end do

    
  end subroutine sortColumns

end program exercise_7_30
