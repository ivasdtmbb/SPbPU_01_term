!____________Array_of_structures_recursive_declaration___________!
implicit none
character(:), allocatable            :: input_file, output_file, &
                                        data_file

integer                              :: STUD_AMOUNT
integer, parameter                   :: SURNAME_LEN   = 15
integer, parameter                   :: INITIALS_LEN  = 5
character(kind=CH_), parameter       :: MALE = Char(1052, CH_), &
                                        FEMALE = Char(1046, CH_)

type student
   character(SURNAME_LEN, kind=CH_)  :: Surname  = ""
   character(INITIALS_LEN, kind=CH_) :: Initials = ""
   character(kind=CH_)               :: Gender   = ""
   integer(I_)                       :: YOB      = 0
end type student

type(student), allocatable           :: Group(:), Boys(:), &
                                        Girls(:)

