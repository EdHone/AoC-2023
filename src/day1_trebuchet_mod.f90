module day1_trebuchet_mod

    use data_kinds_mod, only: str_def, i_def
    use file_mod, only: file_type
    use stdlib_string_type, only: string_type, index

    implicit none

    private
    public :: scan_calibration_values, scan_calibration_values_pt2

    character(*), parameter :: str_num = "0123456789"
    character(*), parameter :: num_str(9) = (/"one  ","two  ","three","four ","five ", &
                                              "six  ","seven","eight","nine "/)

contains

    function scan_data(input_data) result(calibration_value)

        implicit none

        character(str_def), allocatable, intent(in) :: input_data(:)

        character(str_def) :: str_tmp, str_scan
        integer(i_def) :: i, c, n_1_i, n_n_i, n_scan, stat
        integer(i_def) :: calibration_value

        calibration_value = 0
        do i = 1, size(input_data)
            str_tmp = input_data(i)
            n_1_i = scan(str_tmp,str_num)
            n_n_i = len(trim(str_tmp)) - scan(reverse(trim(str_tmp)),str_num) + 1

            str_scan = str_tmp(n_1_i:n_1_i) // str_tmp(n_n_i:n_n_i)
            !end if
            read(str_scan,*,iostat=stat) n_scan

            calibration_value = calibration_value + n_scan

        end do

    end function scan_data

    function format_data(input_data) result(formatted_data)

        implicit none

        character(str_def), allocatable, intent(in) :: input_data(:)
        character(str_def), allocatable :: formatted_data(:)

        type(string_type) :: str_tmp
        integer(i_def) :: i

        print*, index(string_type("4nineeightseven2"), string_type("six"))

        !do i = 1, size(input_data)
        !    str_tmp = string_type(input_data(i))
        !    print*, str_tmp
        !end do

        formatted_data = "R"

    end function format_data

    subroutine scan_calibration_values()

        implicit none

        character(str_def), allocatable :: example_data(:)
        character(str_def), allocatable :: aoc_data(:)
        integer(i_def) :: calibration_value
        type(file_type) :: input_file

        example_data = (/ "1abc2      ", &
                          "pqr3stu8vwx", &
                          "a1b2c3d4e5f", &
                          "treb7uchet " /)

        input_file = file_type("/Users/eh653/Projects/AoC-2023/AoC-2023/data/day1.txt")
        call input_file%open_file()
        aoc_data = input_file%readlines()

        calibration_value = scan_data(aoc_data)
        print*, calibration_value

    end subroutine scan_calibration_values

    subroutine scan_calibration_values_pt2()

        implicit none

        character(str_def), allocatable :: example_data(:)
        character(str_def), allocatable :: aoc_data(:)
        character(str_def), allocatable :: new_data(:)
        character(:), allocatable :: key
        integer(i_def) :: calibration_value
        type(file_type) :: input_file

        example_data = (/ "two1nine        ", &
                          "eightwothree    ", &
                          "abcone2threexyz ", &
                          "xtwone3four     ", &
                          "4nineeightseven2", &
                          "zoneight234     ", &
                          "7pqrstsixteen   " /)

        !input_file = file_type("/Users/eh653/Projects/AoC-2023/AoC-2023/data/day1.txt")
        !call input_file%open_file()
        !aoc_data = input_file%readlines()

        print*, example_data(1)

        key = "two"
        print*, index(example_data(1), "nine")

        new_data = format_data(example_data)
        !print*, calibration_value

    end subroutine scan_calibration_values_pt2

    function reverse(string) result(reverse_string)

        implicit none

        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n

        n = len(string)
        do i = 1, n
            reverse_string(n-i+1:n-i+1) = string(i:i)
        end do

    end function reverse

    function replace(string, charset, target_char) result(res)

        implicit none

        character(*), intent(in) :: string
        character, intent(in) :: charset(:), target_char
        character(len(string)) :: res
        integer :: n
        res = string
        do n = 1, len(string)
            if (any(string(n:n) == charset)) then
                res(n:n) = target_char
            end if
        end do
    end function replace

end module
