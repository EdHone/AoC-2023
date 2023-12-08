module day3_engine_mod

    use data_kinds_mod, only: i_def, str_def
    use file_mod, only: file_type
    use stdlib_string_type
    use string_utils_mod, only: split

    implicit none

    private
    public :: check_engine_parts

    type, public :: engine_number_type
        private
        integer(i_def) :: number
        integer(i_def) :: line_n
        integer(i_def) :: i_start
        integer(i_def) :: i_end
    contains
        procedure, public :: is_part_number
    end type


    character(*), parameter :: str_num = "0123456789"
    character(*), parameter :: str_sym = "!@£$%^&*(#)-_=+[{]}\|';:/?>,<"

contains

    subroutine check_engine_parts()

        implicit none

        type(string_type), allocatable :: day3_data(:)
        type(file_type) :: input_file

        input_file = file_type("/Users/eh653/Projects/AoC-2023/data/day3.txt")
        call input_file%open_file()
        day3_data = input_file%readlines()

        call get_part_numbers(day3_data)


    end subroutine check_engine_parts

    subroutine get_part_numbers(input_data)

        implicit none

        type(string_type), allocatable, intent(in) :: input_data(:)

        type(string_type) :: line
        character(str_def) :: line_char
        integer(i_def) :: i_char, num, num_len, stat, line_n, l_missing, &
                          part_n_tot, dot_l, sym_l
        type(engine_number_type) :: engine_number

        part_n_tot = 0

        do line_n = 1, size(input_data)

            !line_n = 5
            print*, line_n
            line = input_data(line_n)
            line_char = char(line)

            l_missing = 0
            do
                i_char = scan(line_char, str_num)
                if (i_char == 0) exit
                dot_l = scan(line_char(scan(line_char, str_num):len(line_char)), '.')
                sym_l = scan(line_char(scan(line_char, str_num):len(line_char)), str_sym)

                if (dot_l == 0) dot_l = 9999
                if (sym_l == 0) sym_l = 9999

                num_len = min(dot_l, sym_l, len(line_char)-i_char ) - 1

                read(line_char(i_char:min(i_char-1+num_len, len(line_char))), *, iostat=stat) num

                print*, num, line_n, l_missing + i_char, l_missing + i_char + num_len
                engine_number = engine_number_type( num, line_n, &
                                                    l_missing + i_char, &
                                                    l_missing + i_char + num_len )
                if (engine_number%is_part_number(input_data)) then
                    part_n_tot = part_n_tot + engine_number%number
                    print*, engine_number%number
                end if

                line_char = line_char(i_char+num_len : len(line_char))
                l_missing = l_missing + i_char+num_len - 1

                print*, part_n_tot

            end do

        end do

        print*, part_n_tot

    end subroutine get_part_numbers

    function is_part_number(this, dat) result(l_part)

        implicit none

        class(engine_number_type), intent(inout) :: this
        type(string_type), allocatable, intent(in) :: dat(:)

        character(str_def) :: line_above, this_line, line_below
        integer(i_def) :: i
        logical :: l_part

        l_part = .false.

        this_line = char(dat(this%line_n))
        if (this%line_n > 1) then
            line_above = char(dat(this%line_n - 1))
            print*, line_above(this%i_start - 1 : this%i_end)
            if (scan(line_above(this%i_start - 1 : this%i_end), str_sym) /= 0) then
                l_part = .true.
                return
            end if
        end if
        print*, this_line(this%i_start - 1 : this%i_end)
        if (this%line_n < size(dat)) then
            line_below = char(dat(this%line_n + 1))
            print*, line_below(this%i_start - 1 : this%i_end)
            if (scan(line_below(this%i_start - 1 : this%i_end), str_sym) /= 0) then
                l_part = .true.
                return
            end if
        end if
        if (scan(this_line(this%i_start - 1 : this%i_start - 1), str_sym) /= 0) l_part = .true.
        if (scan(this_line(this%i_end : this%i_end), str_sym) /= 0) l_part = .true.

        return

    end function is_part_number

end module day3_engine_mod