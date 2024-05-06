module sbfem_functions
   !! This module contains the functions used in the sbfem code.
   !! The functions are the main functions used for
   !use iso_fortran_env
   use, intrinsic :: iso_c_binding, only: cdp => c_double, cint => c_int, c_ptr, c_loc, c_bool, c_f_pointer
   use functional, only: arange
   use helper_functions, only: point_table
   use math, only: lagrange, lagrange_diff
   implicit none
   private

   public ::  n_vec_f, n_mat_f, r_hat, r, j_mat, j_hat_mat, g_xi, g_eta, n_xi, n_eta, n_vec_f_c, free_n_vec_c

contains



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

   function n_vec_f_c(eta, poly_ord, deriv, n) result(ptr) bind(C)
      real(cdp), intent(in) :: eta
      integer(cint), intent(in) :: poly_ord
      integer(c_bool), intent(in) :: deriv
      integer(cint), intent(out) :: n
      type(c_ptr) :: ptr

      real(cdp), allocatable, target  :: n_vec(:)
      ! NOTE
      ! .... The target attribute in Fortran is used to indicate that a variable
      ! or array may be the target of a pointer assignment.
      ! This is necessary when you want to obtain a C pointer to a Fortran variable or
      ! array using c_loc, as c_loc requires its argument to have a defined memory address
      ! that does not change—something that is not guaranteed for allocatable arrays
      ! or variables without the target attribute.

      ! Allocate n_vec based on poly_ord, fill it, then return its size in 'n'
      ! Example allocation and assignment

      integer(cint) :: i
      real(cdp) :: val
      real(cdp), allocatable :: xm(:)
      n = poly_ord + 1
      allocate(n_vec(n))
      xm  = point_table(poly_ord)
      do i = 1, n
         if (deriv == 0) then
            val = lagrange(eta, i, xm)
         else
            val = lagrange_diff(eta, i, xm)
         end if
         n_vec(i) = val
      end do
      ptr = c_loc(n_vec(1))
   end function n_vec_f_c

   subroutine free_n_vec_c(ptr, n) bind(C)
      use iso_c_binding, only: c_ptr, c_int
      type(c_ptr), intent(in) :: ptr
      integer(c_int), intent(in) :: n
      real(cdp), pointer :: n_vec(:)

      ! Now we can correctly call c_f_pointer with the shape
      call c_f_pointer(ptr, n_vec, [n])
      deallocate(n_vec)
   end subroutine free_n_vec_c



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


   function g_xi(eta, coord_vec, poly_ord, centre) result(g_vec)
      real(cdp), intent(in) :: eta
      real(cdp), intent(in) :: coord_vec(:)
      integer(cint), intent(in) :: poly_ord
      real(cdp), intent(in), optional :: centre(2)
      real(cdp) :: g_vec(2)
      real(cdp), allocatable :: dN(:,:)

      dN = n_mat_f(eta, poly_ord, .true.)
      g_vec = [-dN(2,:) * coord_vec(2), dN(2,:) * coord_vec(1)]
   end function g_xi

   function g_eta(eta, coord_vec, poly_ord, centre) result(g_vec)
      real(cdp), intent(in) :: eta
      real(cdp), intent(in) :: coord_vec(:)
      integer(cint), intent(in) :: poly_ord
      real(cdp), intent(in), optional :: centre(2)
      real(cdp) :: g_vec(2)
      real(cdp) :: r_vec(2)

      r_vec = r(eta, poly_ord, coord_vec, centre)
      g_vec = [-r_vec(2), r_vec(1)]
   end function g_eta

   function n_xi(eta, coord_vec, poly_ord, centre) result(n_vec)
      real(cdp), intent(in) :: eta
      real(cdp), intent(in) :: coord_vec(:)
      integer(cint), intent(in) :: poly_ord
      real(cdp), intent(in), optional :: centre(2)
      real(cdp) :: n_vec(2)
      real(cdp) :: g_vec(2)

      g_vec = g_xi(eta, coord_vec, poly_ord, centre)
      n_vec = g_vec / sqrt(dot_product(g_vec, g_vec))
   end function n_xi

   function n_eta(eta, coord_vec, poly_ord, centre) result(n_vec)
      real(cdp), intent(in) :: eta
      real(cdp), intent(in) :: coord_vec(:)
      integer(cint), intent(in) :: poly_ord
      real(cdp), intent(in), optional :: centre(2)
      real(cdp) :: n_vec(2)
      real(cdp) :: g_vec(2)

      g_vec = g_eta(eta, coord_vec, poly_ord, centre)
      n_vec = g_vec / sqrt(dot_product(g_vec, g_vec))
   end function n_eta

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
