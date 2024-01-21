! Copyright_2023_Spodyneyko_V_Y
! 11-Dec-2023
!-----------------------------------------------------------------------------------------!
module Polynomial_IO
   use Environment

   implicit none

!-------------------------------------------------------------------------!
   type term
      real(R_)                :: coefficient = 0
      integer                 :: exponent = 0
      type(term), allocatable :: next
   end type term

contains

!-------------------------------------------------------------------------!
   ! Чтение полинома из файла в формате  (a n) ... (a, 0)
   function Read_Polynomial(InputFile) result(TermsList)
      type(term), allocatable   :: TermsList
      character(*), intent(in)  :: InputFile
      integer  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         call Read_Term(In, TermsList)
      close (In)
    end function Read_Polynomial

!-------------------------------------------------------------------------!
   ! Чтение следующего члена полинома
    recursive subroutine Read_Term(In, TheTerm)
      type(term), allocatable, intent(out)  :: TheTerm
      integer, intent(in)      :: In
      integer  :: IO

      allocate (TheTerm)
      read (In, '(f5.2, i2)', iostat=IO) TheTerm%coefficient, TheTerm%exponent

      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         call Read_Term(In, TheTerm%next)
      else
         deallocate (TheTerm)
      end if
    end subroutine Read_Term

!-------------------------------------------------------------------------!
   ! Вывод полинома
   subroutine Output_Terms_List(OutputFile, TermsList, ListName, Position)
      character(*), intent(in)             :: OutputFile, Position, ListName
      type(term), allocatable, intent(in)  :: TermsList
      integer  :: Out

      open (file=OutputFile, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') ListName
         call Output_Term(Out, TermsList)
      close (Out)
    end subroutine Output_Terms_List

!-------------------------------------------------------------------------!
    recursive subroutine Output_Term(Out, TheTerm)
      type(term), allocatable, intent(in)  :: TheTerm
      integer, intent(in)                    :: Out
      
      integer  :: IO

      if (Allocated(TheTerm)) then
         write (Out, '(f15.2, 1x, i2)', iostat=IO) TheTerm%coefficient, &
              TheTerm%exponent
         call Handle_IO_status(IO, "writing the Term")
         call Output_Term(Out, TheTerm%next)
      end if
    end subroutine Output_Term

    subroutine Read_X(InputFile, x, derivativeOrder)
      character(*), intent(in) :: InputFile
      real(R_), intent(out)    :: x
      integer, intent(out)     :: derivativeOrder
      integer   :: In

      open (file=InputFile, encoding=E_, newunit=In)
         read(In, *) x
         read(In, *) derivativeOrder
      close(In)
    end subroutine Read_X

    subroutine Output_Value(OutputFile, val, comment, Position)
      character(*), intent(in)  :: OutputFile, Position, comment
      real(R_)                  :: val
      integer                   :: Out

      open (file=OutputFile, encoding=E_, position=Position, newunit=Out)
         write (Out, '(/a)') comment
         write (Out, '(1x, f20.2)') val
      close(Out)
    end subroutine Output_Value
!-----------------------------------------------------------------------------------------!
  end module Polynomial_IO
