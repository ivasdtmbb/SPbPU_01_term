module Chartree_IO
   use Environment

   implicit none

   type node_chartree
      character(len=1, kind=CH_)     :: value = ""
      type(node_chartree), pointer   :: left  => Null()
      type(node_chartree), pointer   :: right => Null()
   end type node_chartree

contains

!--------------------------------------------------------------------------------!
  ! Инициализируем чтение дерева символов из файла
  function Read_CharTree(InputFile) result(CharTree)
    type(node_chartree), pointer  :: CharTree
    character(*), intent(in)      :: InputFile
    integer In

    open (file=InputFile, encoding=E_, newunit=In)
       CharTree => Null()
       call Read_Node_CharTree(In, CharTree)
    close (In)
  end function Read_CharTree

!--------------------------------------------------------------------------------!
  ! Считываем следующий символ для создания узла
  recursive subroutine Read_Node_CharTree(In, CharTree)
    type(node_chartree), pointer, intent(inout)   :: CharTree
    integer, intent(in)                           :: In
    integer                                       :: IO
    character(len=1, kind=CH_)                    :: value = ""

    read (In, '(a1)', iostat=IO, advance='no') value
    call Handle_IO_status(IO, "reading char from file")
    if (IO == 0) then
       call Put_CharTree(value, CharTree)
       call Read_Node_CharTree(In, CharTree)
    end if
  end subroutine Read_Node_CharTree

!--------------------------------------------------------------------------------!
  ! Добавляем узел в тело дерева
  recursive subroutine Put_CharTree(value, current)
    type(node_chartree), pointer, intent(inout)   :: current
    character(len=1, kind=CH_)                    :: value

    if (.not. Associated(current)) then
       allocate (current, source=node_chartree(value))
    else if (value < current%value) then
       call Put_CharTree(value, current%left)
    else if (value > current%value) then
       call Put_CharTree(value, current%right)
    end if
  end subroutine Put_CharTree
  
!--------------------------------------------------------------------------------!
  subroutine Output_CharTree(Output_File, CharTree, OperationName, Position)
    character(*), intent(in)      :: Output_File, Position, OperationName
    type(node_chartree), pointer  :: CharTree
    integer  :: Out

    open (file=Output_File, encoding=E_, position=Position, newunit=Out)
       write (out, '(/a)') OperationName
       if (Associated(CharTree)) &
            call Output_CharTree_Node(Out, CharTree)
    close (Out)
  end subroutine Output_CharTree

!--------------------------------------------------------------------------------!
  recursive subroutine Output_CharTree_Node(Out, current)
    integer, intent(in)              :: Out
    type(node_chartree), intent(in)  :: current
    integer  :: IO

    write (Out, '(a1, 1x)', advance='no', iostat=IO) current%value
    call Handle_IO_status(IO, "writing tree")
    
    if (Associated(current%left)) &
         call Output_CharTree_node(Out, current%left)
    if (Associated(current%right)) &
         call Output_CharTree_node(Out, current%right)

  end subroutine Output_CharTree_Node

end module Chartree_IO
