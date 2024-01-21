!________________Structure_of_arrays_declaration_________________!
implicit none
character(:), allocatable           :: input_file, output_file, &
                                       data_file
integer                             :: STUD_AMOUNT
integer, parameter                  :: SURNAME_LEN = 15
integer, parameter                  :: INITIALS_LEN  = 5
character(kind=CH_), parameter      :: MALE = Char(1052, CH_), &
                                       FEMALE = Char(1046, CH_)

type students
   character(SURNAME_LEN,kind=CH_),allocatable  :: Surnames(:)
   character(INITIALS_LEN,kind=CH_),allocatable :: Initials(:)
   character(kind=CH_), allocatable             :: Genders(:)
   integer(I_), allocatable                     :: YOBs(:)
end type students

type(students) :: Group, Male_Students, Female_Students

