module Persons_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15

!-------------------------------------------------------------------------!
   type person
      character(SURNAME_LEN, kind=CH_)  :: Surname = ""
      type(person), allocatable         :: next
   end type person

contains

!-------------------------------------------------------------------------!
   ! Чтение списка людей
   function Read_Persons_List(Input_File) result(Persons_List)
      type(person), allocatable  :: Persons_List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, encoding=E_, newunit=In)
         call Read_Person(In, Persons_List)
      close (In)
    end function Read_Persons_List

!-------------------------------------------------------------------------!
   ! Чтение следующего человека.
   recursive subroutine Read_Person(In, ThePerson)
      type(person), allocatable  :: ThePerson
      integer, intent(in)        :: In
      integer  IO
      
      allocate (ThePerson)
      read (In, '(a)', iostat=IO) ThePerson%Surname
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         call Read_Person(In, ThePerson%next)
      else
         deallocate (ThePerson)
      end if
    end subroutine Read_Person

!-------------------------------------------------------------------------!
   ! Вывод списка людей.
   subroutine Output_Persons_List(OutputFile, PersonsList, ListName, Position)
      character(*), intent(in)               :: OutputFile, Position, ListName
      type(person), allocatable, intent(in)  :: PersonsList
      integer  :: Out

      open (file=OutputFile, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') ListName
         call Output_Person(Out, PersonsList)
      close (Out)
    end subroutine Output_Persons_List

!-------------------------------------------------------------------------!
   recursive subroutine Output_Person(Out, ThePerson)
      type(person), allocatable, intent(in)  :: ThePerson
      integer, intent(in)                    :: Out
      
      integer  :: IO

      if (Allocated(ThePerson)) then
         write (Out, '(a)', iostat=IO) ThePerson%Surname
         call Handle_IO_status(IO, "writing the Person")
         call Output_Person(Out, ThePerson%next)
      end if
    end subroutine Output_Person

  end module Persons_IO
