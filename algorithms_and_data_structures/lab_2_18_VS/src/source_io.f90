module Source_IO
   use Environment

   implicit none
   character(kind=CH_), parameter   :: Period = '.'
   character(kind=CH_), parameter   :: Asterisk = '*'
   ! Структура данных для хранения имени файла и его расширения
   type FileName
      character(:, CH_), allocatable   :: Name
      character(:, CH_), allocatable   :: Extension
      type(FileName), pointer          :: Next  => Null()
   end type FileName

contains

!-----------------------------------------------------------------------------!
   ! Чтение маски файлов или списка имён файлов
   function Read_FN_Records(InputFile) result(FN_Record)
      type(FileName), pointer    :: FN_Record
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         FN_Record => Read_Next_FN_Record(in)
      close (In)
    end function Read_FN_Records

!-----------------------------------------------------------------------------!
   ! Чтение строки исходного кода.
   recursive function Read_Next_FN_Record(in) result(FN_Record)
      type(FileName), pointer          :: FN_Record
      integer, intent(in)              :: In
      integer, parameter               :: max_len = 1024      
      character(max_len, CH_)          :: FileNameString
      integer                          :: IO, PeriodIndex
      
      ! Чтение имени файла
      read (In, "(a)", iostat=IO) FileNameString
      
      PeriodIndex = Index(FileNameString, Period)

      call Handle_IO_Status(IO, "reading filename from the source file")
      if (IO == 0) then
         allocate (FN_Record)
         FN_Record%Name = FileNameString(1 : (PeriodIndex-1))
         FN_Record%Extension = Trim(FileNameString((PeriodIndex + 1) : &
              Len(FileNameString)))
         FN_Record%Next => Read_Next_FN_Record(In)
      else
         FN_Record => Null()
      end if
    end function Read_Next_FN_Record

 !----------------------------------------------------------------------------!
    ! Вывод имён файлов
    subroutine Output_File_Names(OutputFile, FN_Record, Position)
      character(*), intent(in)      :: OutputFile, Position
      type(FileName), intent(in)  :: FN_Record
      integer  :: Out

      open (file=OutputFile, encoding=E_, position=Position, newunit=Out)
      call Output_Next_FN_Record(Out, FN_Record)
      close (Out)
    end subroutine Output_File_Names

 !----------------------------------------------------------------------------!
    ! Вывод строки исходного кода.
    recursive subroutine Output_Next_FN_Record(Out, FN_Record)
      integer, intent(in)           :: Out
      type(FileName), intent(in)    :: FN_Record
      integer  :: IO

      write (Out, "(a)", iostat=IO) FN_Record%Name // Period // &
           FN_Record%Extension
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(FN_Record%next)) &
           call Output_Next_FN_Record(Out, FN_Record%next)
    end subroutine Output_Next_FN_Record
    
end module Source_IO
