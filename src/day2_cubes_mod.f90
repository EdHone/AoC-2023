module day2_cubes_mod

    use data_kinds_mod, only: i_def, str_def
    use file_mod, only: file_type
    use stdlib_string_type, only: string_type, char
    use stdlib_strings, only: find, count, slice

    implicit none

    private
    public :: check_game_possibility

contains

    subroutine check_game_possibility()

        implicit none

        type(string_type), allocatable :: example_data(:)
        type(string_type) :: tmp_str
        character(len=str_def) :: tmp_char
        character(str_def), allocatable :: aoc_data(:)
        type(file_type) :: input_file
        integer(i_def) :: l

        integer(i_def) :: viable_game_index = 0
        integer(i_def) :: total_game_power = 0

        example_data = [string_type("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"), &
                        string_type("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"), &
                        string_type("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"), &
                        string_type("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"), &
                        string_type("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")]

        input_file = file_type("/Users/eh653/Projects/AoC-2023/AoC-2023/data/day2.txt")
        call input_file%open_file()
        aoc_data = input_file%readlines()

        print*, game_is_possible(string_type(aoc_data(94)))

        do l = 1, size(aoc_data)
            if (game_is_possible(string_type(aoc_data(l)))) then
                viable_game_index = viable_game_index + l
            end if
            total_game_power = total_game_power + get_power_set(string_type(aoc_data(l)))
        end do

        print*, viable_game_index
        print*, total_game_power

    end subroutine check_game_possibility

    function get_max_cubes(input_str, cube_variant) result(n_max_cubes)

        implicit none

        type(string_type), intent(in) :: input_str
        type(string_type), intent(in) :: cube_variant

        integer(i_def) :: i, i_n, n_cubes, n_max_cubes, stat
        character(2) :: n_cubes_str

        n_max_cubes = 0
        do i = 1, count(input_str, cube_variant)
            i_n = find(input_str, cube_variant, i) - 2
            n_cubes_str = char(slice(input_str, i_n - 1, i_n + 1))
            read(n_cubes_str, *, iostat=stat) n_cubes

            if (n_cubes > n_max_cubes) n_max_cubes = n_cubes

        end do

    end function get_max_cubes

    logical function game_is_possible(game)

        implicit none

        type(string_type), intent(in) :: game

        integer(i_def), parameter :: max_red = 12
        integer(i_def), parameter :: max_green = 13
        integer(i_def), parameter :: max_blue = 14

        game_is_possible = .false.
        if (get_max_cubes(game, string_type("red")) <= max_red) then
            if (get_max_cubes(game, string_type("green")) <= max_green) then
                if (get_max_cubes(game, string_type("blue")) <= max_blue) then
                    game_is_possible = .true.
                end if
            end if
        end if

    end function game_is_possible


    function get_power_set(game) result(power)

        implicit none

        type(string_type), intent(in) :: game
        integer(i_def) :: r, g, b, power

        r = get_max_cubes(game, string_type("red"))
        g = get_max_cubes(game, string_type("green"))
        b = get_max_cubes(game, string_type("blue"))

        power = r * g * b

    end function get_power_set

end module day2_cubes_mod