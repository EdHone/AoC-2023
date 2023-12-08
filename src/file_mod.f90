module file_mod

    use data_kinds_mod, only: i_def, str_def
    use stdlib_string_type

    implicit none

    private

    type, public :: file_type
        private
        character(str_def)  :: path
        integer(kind=i_def) :: unit = 9
    contains
        procedure, public :: open_file
        procedure, public :: readlines
        procedure, public :: n_lines
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

        type(string_type), allocatable :: contents(:)
        character(str_def) :: buffer
        integer :: l, data_len

        data_len = self%n_lines()
        allocate(contents(data_len))

        open(unit=self%unit, file=self%path, action='read')

        do l = 1, data_len
            read(self%unit, '(A)') buffer
            contents(l) = string_type(buffer)
        enddo

        close(self%unit)

    end function readlines

    function n_lines(self) result(nlines)

        implicit none

        class(file_type), intent(in) :: self

        integer :: l, nlines, io

        open(unit=self%unit, file=self%path, action='read')

        nlines = 0
        do
            read(self%unit, *, iostat=io)
            if (io == 0) then
                nlines = nlines + 1
            else
                exit
            end if
        end do

        close(self%unit)

    end function n_lines

end module file_mod
