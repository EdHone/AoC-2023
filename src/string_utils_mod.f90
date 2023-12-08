module string_utils_mod

    use data_kinds_mod, only: i_def
    use stdlib_string_type
    use stdlib_stringlist_type

    implicit none

    private
    public :: split

contains

    !> Splits a string into a list of strings based on an specified delimiter
    !
    !> param[in] input_str The string to split
    !> param[in] delim     The string delimiter
    !!
    !> returns   split_string_list A stringlist type containing the split
    !!                             elements of the input string
    function split(input_str, delim) result(split_string_list)

        implicit none

        type(string_type), intent(in) :: input_str
        type(string_type), intent(in) :: delim

        type(string_type)     :: scan_str, add_str
        type(stringlist_type) :: split_string_list
        integer(i_def)        :: delim_index

        split_string_list = stringlist_type()
        scan_str = input_str

        do
            delim_index = scan(scan_str, delim)
            if (delim_index == 0) then
                split_string_list = split_string_list // scan_str
                exit
            else if (delim_index == len_trim(scan_str)) then
                add_str = string_type(char(scan_str, 1, delim_index - 1))
                split_string_list = split_string_list // add_str
                exit
            else
                add_str = string_type(char(scan_str, 1, delim_index - 1))
                split_string_list = split_string_list // add_str
                scan_str = string_type(trim(adjustl( &
                            char(scan_str, delim_index + 1, len(scan_str)))))
            end if
        end do

    end function split

end module string_utils_mod