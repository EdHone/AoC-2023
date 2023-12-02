module data_kinds_mod

use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, &
                                          real32, real64, real128

    implicit none

    private
    public :: i_def, r_def, str_def

    integer, parameter :: i_def = int32  !< Default integer kind
    integer, parameter :: r_def = real32 !< Default real kind
    integer, parameter :: str_def = 256  !< Default string length

end module data_kinds_mod
