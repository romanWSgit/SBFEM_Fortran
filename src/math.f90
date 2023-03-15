module math
    use iso_fortran_env
    ! use funtional
    ! use helper_functions, only: append_real64
    implicit none
    private

    public :: lagrange, lagrange_diff
  
    contains

    real(real64) function lagrange(x, i, xm) result(y)
        !! Evaluates the i-th Lagrange polynomial at x based on grid data xm
        real(real64), intent(in) :: x
        integer, intent(in) :: i
        real(real64), intent(in) :: xm(:)
        integer :: xm_size
        integer :: j
        integer :: n
        xm_size = size(xm)
        n = xm_size
        y = 1.0

        do j = 1, n
            if (i /= j) then
                y = y * (( x - xm(j) ) / ( xm(i) - xm(j) ))
            end if
        end do
    end function lagrange

    real(real64) function lagrange_diff(x, i, xm) result(y)
        !! Evaluates the i-th Lagrange polynomial at x based on grid data xm
        real(real64), intent(in) :: x
        integer, intent(in) :: i
        real(real64), intent(in) :: xm(:)
        real(real64):: k
        integer :: xm_size
        integer :: m, l
        integer :: n
        xm_size = size(xm)
        n = xm_size
        y = 0.0
        k = 0.0
        do l = 1, n
            if (l /= i) then
                k = 1 / (xm(i) - xm(l))
                do m = 1, n
                    if ((m /= i) .and. (m /= l)) then
                        k = k * (x - xm(m)) / (xm(i) - xm(m))
                    end if
                end do
                y = y + k
            end if
        end do
    end function lagrange_diff


end module math