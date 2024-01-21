module Chartree_IO
   use Environment

   implicit none

   type node_chartree
      character(len=1, kind=CH_)       :: value = ""
      integer                          :: depth = 0
      type(node_chartree), allocatable :: left
      type(node_chartree), allocatable :: right
   end type node_chartree

   type queue_chartree
      type(node_chartree),  pointer     :: node
      type(queue_chartree), allocatable :: next 
   end type queue_chartree

contains

!--------------------------------------------------------------------------------!
  ! Инициализируем чтение дерева символов из файла
  function Read_CharTree(InputFile) result(CharTree)
    type(node_chartree), allocatable :: CharTree
    character(*), intent(in)         :: InputFile
    integer In

    open (file=InputFile, encoding=E_, newunit=In)
       call Read_Node_CharTree(In, CharTree)
    close (In)
  end function Read_CharTree

!--------------------------------------------------------------------------------!
  ! Считываем следующий символ для создания узла
  recursive subroutine Read_Node_CharTree(In, CharTree)
    type(node_chartree), allocatable, intent(inout) :: CharTree
    integer, intent(in)                             :: In
    integer                                         :: IO
    character(len=1, kind=CH_)                      :: value = ""
    integer, parameter                              :: depth = 0

    read (In, '(a1)', iostat=IO, advance='no') value
    call Handle_IO_status(IO, "reading char from file")
    if (IO == 0) then
       call Put_CharTree(value, CharTree, depth)
       call Read_Node_CharTree(In, CharTree)
    end if
  end subroutine Read_Node_CharTree

!--------------------------------------------------------------------------------!
  ! Добавляем узел в тело дерева
  recursive subroutine Put_CharTree(value, current, depth)
    type(node_chartree), allocatable, intent(inout) :: current
    character(len=1, kind=CH_)                      :: value
    integer, intent(in)                             :: depth

    if (.not. Allocated(current)) then
       allocate (current, source=node_chartree(value, depth))
    else if (value < current%value) then
       call Put_CharTree(value, current%left, depth+1)
    else if (value > current%value) then
       call Put_CharTree(value, current%right, depth+1)
    end if
  end subroutine Put_CharTree
  
!--------------------------------------------------------------------------------!
  subroutine Output_CharTree(Output_File, CharTree, OperationName, Position)
    character(*), intent(in) :: Output_File, Position, OperationName
    type(node_chartree), allocatable, intent(in) :: CharTree
    integer  :: Out

    open (file=Output_File, encoding=E_, position=Position, newunit=Out)
       write (out, '(/a)') OperationName
       if (Allocated(CharTree)) &
            call Output_CharTree_Node(Out, CharTree)
    close (Out)
  end subroutine Output_CharTree

!--------------------------------------------------------------------------------!
  recursive subroutine Output_CharTree_Node(Out, current)
    integer, intent(in)             :: Out
    type(node_chartree), intent(in) :: current
    integer  :: IO

    write (Out, '(a1, 1x)', advance='no', iostat=IO) current%value
    call Handle_IO_status(IO, "writing tree")

    if (Allocated(current%left)) &
         call Output_CharTree_node(Out, current%left)

    if (Allocated(current%right)) &
         call Output_CharTree_node(Out, current%right)
  end subroutine Output_CharTree_Node

!--------------------------------------------------------------------------------!
  subroutine Output_CharTree_Width(Output_File, CharTree, OperationName, Position)
    character(*), intent(in) :: Output_File, Position, OperationName
    type(node_chartree),  target, allocatable, intent(in) :: CharTree
    type(queue_chartree), allocatable                     :: Queue
    integer :: Out, depth=-1
    
    open (file=Output_File, encoding=E_, position=Position, newunit=Out)
       write (out, '(/a)') OperationName
       if (Allocated(CharTree)) then
          allocate (Queue)
          Queue%node => CharTree
          call Output_CharTree_Node_Width(Out, Queue, depth)
       end if
    close (Out)
  end subroutine Output_CharTree_Width
! !--------------------------------------------------------------------------------!
  recursive subroutine Output_CharTree_Node_Width(Out, Queue, depth)
    integer, intent(in)                              :: Out
    type(queue_chartree), allocatable, intent(inout) :: Queue
    type(queue_chartree), allocatable                :: firstInQueue
    integer, intent(inout)                           :: depth
    integer :: IO

    if (Allocated(Queue)) then
       firstInQueue = Dequeue(Queue)
       if (firstInQueue%node%depth > depth) then
          depth = depth + 1

          write (Out, '(/, i3, a2)', advance='no', iostat=IO) firstInQueue%node%depth, "| "
          write (Out, '(a1, 1x)', advance='no', iostat=IO) firstInQueue%node%value
          call Handle_IO_status(IO, "writing tree")
       else
          write(Out, '(a1, 1x)', advance='no', iostat=IO) firstInQueue%node%value
          call Handle_IO_status(IO, "writing tree")
       end if
       
       if (allocated (firstInQueue%node%left)) &
            call Put_In_Queue(Queue, firstInQueue%node%left)
       if (allocated (firstInQueue%node%right)) &
            call Put_In_Queue(Queue, firstInQueue%node%right)

       Deallocate(firstInQueue)
       call Output_CharTree_Node_Width(Out, Queue, depth)
    end if

  end subroutine Output_CharTree_Node_Width

! !--------------------------------------------------------------------------------!
  recursive subroutine Put_In_Queue(currentElement, newElement)
    type(queue_chartree), allocatable, intent(inout) :: currentElement
    type(node_chartree), target, intent(in)          :: newElement

    if (.not. Allocated(currentElement)) then
       Allocate (currentElement, source=queue_chartree(newElement))
    else
       call Put_In_Queue(currentElement%next, newElement)
    end if
  end subroutine Put_In_Queue

!--------------------------------------------------------------------------------!
  function Dequeue(Queue) result(element)
    type(queue_chartree), allocatable, intent(inout) :: Queue
    type(queue_chartree), allocatable                :: element

    call Move_Alloc(Queue, element)
    call Move_Alloc(element%next, Queue)

  end function Dequeue
!--------------------------------------------------------------------------------! 

  
end module Chartree_IO

 
