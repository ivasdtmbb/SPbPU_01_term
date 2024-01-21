!________________Dynamic_list_declaration________________________!
implicit none
character(:), allocatable            :: input_file, output_file
character(kind=CH_), parameter       :: MALE = Char(1052, CH_), &
                                        FEMALE = Char(1046, CH_)
integer, parameter                   :: SURNAME_LEN   = 15
integer, parameter                   :: INITIALS_LEN  = 5
integer, parameter                   :: MARKS_AMOUNT  = 5

type student
   character(SURNAME_LEN, kind=CH_)  :: Surname  = ""
   character(INITIALS_LEN, kind=CH_) :: Initials = ""
   character(kind=CH_)               :: Gender   = ""
   Integer(R_)                       :: YOB      = 0
   type(student), pointer            :: next => Null()
end type student

type(student), pointer               :: Group_List => Null(), &
                                        Boys_List => Null(), &
                                        Girls_List => Null()
