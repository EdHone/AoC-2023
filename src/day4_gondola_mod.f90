module day4_gondola_mod

    use data_kinds_mod, only: i_def, str_def
    use file_mod, only: file_type
    use stdlib_string_type
    use stdlib_stringlist_type
    use string_utils_mod, only: split

    implicit none

    private
    public :: total_scratchcard_points

contains

    subroutine total_scratchcard_points()

        implicit none

        type(string_type) :: input_str, n_entry
        type(string_type), allocatable :: input_data(:)
        type(stringlist_type) :: split_str, n_winning
        integer(i_def) :: d, i, n_index, score, total_score
        type(file_type) :: input_file

        input_file = file_type("/Users/eh653/Projects/AoC-2023/data/day4.txt")
        call input_file%open_file()
        input_data = input_file%readlines()

        total_score = 0

        do d = 1, size(input_data)

            input_str = input_data(d)

            split_str = split(input_str, string_type(":"))
            split_str = split(split_str%get(bidx(1)), string_type("|"))

            n_winning = split(split_str%get(fidx(1)), string_type(" "))
            n_entry   = split_str%get(fidx(2))

            score = 0
            do i = 1, n_winning%len()
                n_index = index(char(n_entry), char(n_winning%get(fidx(i))))
                if (n_index > 0) then
                    if (score == 0) then
                        score = 1
                    else
                        score = score * 2
                    end if
                end if
            end do

            total_score = total_score + score

        end do

        print*, total_score

    end subroutine total_scratchcard_points

end module day4_gondola_mod