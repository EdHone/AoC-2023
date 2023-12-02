module file_mod

    use data_kinds_mod, only: i_def, str_def

    implicit none

    private

    type, public :: file_type
        private
        character(str_def)  :: path
        integer(kind=i_def) :: unit = 99
    contains
        procedure, public :: open_file
        procedure, public :: readlines
    end type file_type

    interface file_type
        module procedure file_constructor
    end interface

contains

    function file_constructor(path) result(self)

        implicit none

        type(file_type) :: self
        character(len=*), intent(in) :: path

        self%path = trim(path)

        return

    end function file_constructor


    subroutine open_file(self)

        implicit none

        class(file_type), intent(in) :: self

        open(unit=self%unit, file=self%path, action='read')

    end subroutine open_file

    function readlines(self) result(contents)

        implicit none

        class(file_type), intent(in) :: self

        character(len=str_def), dimension(:), allocatable :: contents
        integer :: l, n_lines

        open(unit=self%unit, file=self%path, action='read')
        !read(self%unit, *), n_lines
        n_lines = 100
        allocate(contents(n_lines))

        do l = 1, n_lines
           read(self%unit, '(A)') contents(l)
        enddo

    end function readlines

end module file_mod
