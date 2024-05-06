module sbfem_functions
    !use iso_fortran_env
    use, intrinsic :: iso_c_binding, only: cdp => c_double, cint => c_int 
    use functional, only: arange
    use helper_functions, only: point_table
    use math, only: lagrange, lagrange_diff
    implicit none
    private

    public ::  n_vec_f, n_mat_f, r_hat, r, j_mat, j_hat_mat
  
    contains

    !! Computes the shape function vector or its derivative for a 
    !! given polynomial order and evaluation point.
    function n_vec_f(eta, poly_ord, deriv) result(n_vec)
        !! Computes the shape function vector or its derivative for a 
        !!given polynomial order and evaluation point.

        ! Arguments
        real(cdp),  intent(in) :: eta
            !! The natural coordinate (real number) at which the shape function 
            !! or its derivative is evaluated.
        integer(cint),  intent(in) :: poly_ord
            !! The natural coordinate (real number) at which the shape function 
        logical,  intent(in) :: deriv
            !! A logical type which determines whether the shape function or its
            !! derivative is computed. If deriv is true, the derivative is computed.
        real(cdp) :: n_vec(poly_ord + 1)
            !! The shape function vector or its derivative.
        real(cdp) :: val 
        integer(cint) :: i
        real(cdp), allocatable :: xm(:)
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
        real(cdp),  intent(in) :: eta
        integer(cint),  intent(in) :: poly_ord
        logical,  intent(in) :: deriv
        real(cdp) :: n_mat(2, (poly_ord + 1) * 2)
        real(cdp) :: val 
        integer(cint) :: i, j ! loop variables
        real(cdp), allocatable :: xm(:)
        integer(cint), allocatable :: range_list(:)
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
            n_mat(1,j + 1) = 0._cdp
            n_mat(2,j) = 0._cdp
            n_mat(2,j + 1) = val
        end do
    end function n_mat_f

    function r_hat(xi, eta, poly_ord, coord_vec, centre) result(r_vec)
        !! create the shape function Vector and Matrix N
        real(cdp), intent(in) :: xi
        real(cdp), intent(in) :: eta
        integer(cint),  intent(in) :: poly_ord
        real(cdp), intent(in) :: coord_vec(:)
        real(cdp), optional, intent(in) :: centre(2)
        real(cdp) :: r_vec(2)
        real(cdp) :: cen(2)
        if(present(centre)) then
            cen=centre
        else
            cen=[0._cdp, 0._cdp]
        endif

        r_vec = xi * matmul(n_mat_f(eta, poly_ord, .false.), coord_vec) + cen
    
    end function r_hat

    function r(eta, poly_ord, coord_vec, centre) result(r_vec)
        !! create the shape function Vector and Matrix N
        real(cdp), intent(in) :: eta
        integer(cint),  intent(in) :: poly_ord
        real(cdp), intent(in) :: coord_vec(:)
        real(cdp), optional, intent(in) :: centre(2)
        real(cdp) :: r_vec(2)
        real(cdp) :: cen(2)
        if(present(centre)) then
            cen=centre
        else
            cen=[0._cdp, 0._cdp]
        endif

        r_vec = matmul(n_mat_f(eta, poly_ord, .false.), coord_vec) + cen
    
    end function r
	
    function j_mat(eta, coord_vec, poly_ord, shape_function_type, centre) result (j_matrix)
        ! Arguments
        real(cdp), intent(in) :: eta
        real(cdp), intent(in), dimension(2) :: coord_vec
        integer(cint), intent(in) :: poly_ord
        character(len=:), allocatable, intent(in) :: shape_function_type
        real(cdp), dimension(2,2) :: j_matrix
        real(cdp), intent(in), dimension(2) :: centre
        real(cdp), dimension(2) :: r_result
        real(cdp), dimension(2) :: shape_dN_result
	    ! Call the r function
        r_result = r(eta, poly_ord, coord_vec, centre)

	    ! Call the shape_dN function
        shape_dN_result =n_vec_f(eta, poly_ord, .true.)

	    ! Populate the matrix
        j_matrix(1,1) = r_result(1)
        j_matrix(1,2) = r_result(2)
        j_matrix(2,1) = dot_product(shape_dN_result, coord_vec)
        j_matrix(2,2) = dot_product(shape_dN_result, coord_vec)


    end function j_mat
	
    ! J_Hat Matrix function
    function j_hat_mat(xi, eta, coord_vec, poly_ord, shape_function_type, centre) result (j_hat_matrix)
        ! Arguments
        real(cdp), intent(in) :: xi, eta
        real(cdp), intent(in), dimension(2) :: coord_vec
        integer(cint), intent(in) :: poly_ord
        character(len=:), allocatable, intent(in) :: shape_function_type
        real(cdp), dimension(2,2) :: j_hat_matrix
        real(cdp), intent(in), dimension(2) :: centre
        real(cdp), dimension(2) :: r_result
        real(cdp), dimension(2) :: shape_dN_result

        ! Call the r function
        r_result = r(eta, poly_ord, coord_vec, centre)

        ! Call the shape_dN function
        shape_dN_result =n_vec_f(eta, poly_ord, .true.)

        ! Populate the matrix
        j_hat_matrix(1,1) = r_result(1)
        j_hat_matrix(1,2) = r_result(2)
        j_hat_matrix(2,1) = xi * dot_product(shape_dN_result, coord_vec)
        j_hat_matrix(2,2) = xi * dot_product(shape_dN_result, coord_vec)

    end function j_hat_mat

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