module sbfem_functions
    use iso_fortran_env
    use functional, only: arange
    use helper_functions, only: point_table
    use math, only: lagrange, lagrange_diff
    implicit none
    private

    public ::  n_vec_f, n_mat_f, r_hat, r
  
    contains

    function n_vec_f(eta, poly_ord, deriv) result(n_vec)
        !! create the shape function Vector and Matrix N
        real(real64),  intent(in) :: eta
        integer,  intent(in) :: poly_ord
        logical,  intent(in) :: deriv
        real(real64) :: n_vec(poly_ord + 1)
        real(real64) :: val 
        integer :: i
        real(real64), allocatable :: xm(:)
        xm  = point_table(poly_ord)
        do i = 1, poly_ord + 1
            if (deriv .eqv. .false.) then
                val = lagrange(eta, i, xm)
            else
                val = lagrange_diff(eta, i, xm)    
            end if
            n_vec(i) = val
        end do

    end function n_vec_f

    function n_mat_f(eta, poly_ord, deriv) result(n_mat)
        !! create the shape function Vector and Matrix N
        real(real64),  intent(in) :: eta
        integer,  intent(in) :: poly_ord
        logical,  intent(in) :: deriv
        real(real64) :: n_mat(2, (poly_ord + 1) * 2)
        real(real64) :: val 
        integer :: i, j ! loop variables
        real(real64), allocatable :: xm(:)
        integer, allocatable :: range_list(:)
        xm  = point_table(poly_ord) 
        range_list = arange(0, poly_ord + 1)
        do i = 1, (poly_ord + 1)
            j = i + range_list(i)
            if (deriv .eqv. .false.) then
                val = lagrange(eta, i, xm)
            else
                val = lagrange_diff(eta, i, xm)    
            end if
            n_mat(1,j) = val
            n_mat(1,j + 1) = 0._real64
            n_mat(2,j) = 0._real64
            n_mat(2,j + 1) = val
        end do
    end function n_mat_f


    function r_hat(xi, eta, poly_ord, coord_vec, centre) result(r_vec)
        !! create the shape function Vector and Matrix N
        real(real64), intent(in) :: xi
        real(real64), intent(in) :: eta
        integer,  intent(in) :: poly_ord
        real(real64), intent(in) :: coord_vec(:)
        real(real64), optional, intent(in) :: centre(2)
        real(real64) :: r_vec(2)
        real(real64) :: cen(2)
        if(present(centre)) then
            cen=centre
        else
            cen=[0._real64, 0._real64]
        endif

        r_vec = xi * matmul(n_mat_f(eta, poly_ord, .false.), coord_vec) + cen
    
    end function r_hat

    function r(eta, poly_ord, coord_vec, centre) result(r_vec)
        !! create the shape function Vector and Matrix N
        real(real64), intent(in) :: eta
        integer,  intent(in) :: poly_ord
        real(real64), intent(in) :: coord_vec(:)
        real(real64), optional, intent(in) :: centre(2)
        real(real64) :: r_vec(2)
        real(real64) :: cen(2)
        if(present(centre)) then
            cen=centre
        else
            cen=[0._real64, 0._real64]
        endif

        r_vec = matmul(n_mat_f(eta, poly_ord, .false.), coord_vec) + cen
    
    end function r

!     def r_hat_c(xi, eta, poly_ord, coord_vec, centre=np.array([0, 0])):
!     return xi * np.dot(shape_N(eta, poly_ord)[1], coord_vec) + centre


! def r_hat(xi, eta, coord_vec, poly_ord):
!     return xi * np.dot(shape_N(eta, poly_ord)[1], coord_vec)


! def r_c(eta, poly_ord, coord_vec, shape_function_type, centre=np.array([0, 0])):
!     if shape_function_type == "standard shape functions":
!         return (r_hat_c(1, eta, coord_vec, poly_ord, centre) - centre)
!     elif shape_function_type == "hierarchical shape functions":
!         return (r_hat_c(1, eta, coord_vec, poly_ord, centre) - centre)  # JET TO IMPLEMENT !!!!!
!     else:
!         print("ERROR: No valid shape function type in FUNCTION  rc(eta,coord_vec,centre)")
!         return np.array([0, 0])


! def r(eta, poly_ord, coord_vec, shape_function_type, centre=np.array([0, 0])):
!     if shape_function_type == "standard shape functions":
!         return r_hat(1, eta, coord_vec, poly_ord)
!     elif shape_function_type == "hierarchical shape functions":
!         return r_hat(1, eta, coord_vec, poly_ord)  # JET TO IMPLEMENT !!!!!
!     else:
!         print("ERROR: No valid shape function type in FUNCTION -r(eta,coord_vec)")
!         return np.array([0, 0])

    

end module sbfem_functions