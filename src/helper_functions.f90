module helper_functions
    use json_module
    use iso_fortran_env
    ! use funtional
    implicit none
    private
  
    public :: append_int, append_real64, point_table, print_matrix, &
                print_column_vector, read_matrix_from_json

    contains

    function point_table(poly_ord) result(p_table)
        integer, intent(in) :: poly_ord
        real(real64), allocatable :: p_table(:)
        real(real64) :: res
        integer :: j
        allocate(p_table(poly_ord + 1))
        do j = 1, (poly_ord + 1)
            res = -1._real64 + (j - 1._real64) * 2._real64 / poly_ord
            p_table(j) = res
        end do
    end function point_table


    subroutine append_int(vec, val)
        !***********************************************************************
        !> \brief Appends val in vec if not already present
        !> \date 05 2020
        !***********************************************************************
        integer, allocatable, intent(inout)    :: vec(:)
        integer, intent(in)                    :: val
        vec = [vec, val]
    end subroutine append_int

    subroutine append_real64(vec, val)
        !***********************************************************************
        !> \brief Appends val in vec if not already present
        !> \date 05 2020
        !***********************************************************************
        real(real64), allocatable, intent(inout)    :: vec(:)
        real(real64), intent(in)                    :: val
        vec = [vec, val]
    end subroutine append_real64

    subroutine print_matrix(a,m,n)
	    !! Our amazing subroutine to say hello (FORD)
	    !-----print_matrix-----------------------------------
	    !
	    !  print the matrix A with dimensions to the console m,n
	    !
	    !----------------------------------------------------
	    !! Who to say hello to
        integer, intent(in) :: n
        integer, intent(in) :: m
        real(real64), intent(in) :: a(m,n)
        integer :: i,j
        print*, ''
        do i=1,m
            print *, (a(i,j), j=1,n)
        end do
        print*, ''
    end subroutine print_matrix

    subroutine print_column_vector(a,m)
        !-----print_column_vector-----------------------------------
        !
        !  Subroutine to print out an array of dimension m,n
        !
        !----------------------------------------------------
        integer, intent(in) :: m
        real(real64), intent(in) :: a(m)
        integer :: i
        print*, ''
        do i=1,m
            print *, a(i)
        end do
        print*, ''
    end subroutine print_column_vector

    function read_matrix_from_json(object, path) result(imat)
        Character(*), intent(in) :: object
        Character(*), intent(in) :: path
        
        type(json_file) :: json
        real(real64), allocatable :: ivec(:)
        real(real64), allocatable :: imat(:,:)
        integer :: i,n_cols,n_rows,var_type
        logical :: found
        type(json_value),pointer :: matrix_to_read, child
        type(json_core) :: core
    
        !load the file and print it to console:
        call json%load_file(path)
        if (json%failed()) error stop 'error loading file'
    
        !get number of rows and columns
        !assuming data stored by column
        !assuming each column has the same number of elements,
        !and is the same data type (integer in this case):
        call json%info(object,found,var_type,n_cols)
        if (.not. found) error stop 'error: matrix_to_read not found'
    
        call json%info(object//'(1)',found,var_type,n_rows)
        if (.not. found) error stop "object(1) not found"
    
        !get a pointer to the wind matrix:
        call json%get(object,matrix_to_read)
        if (.not. associated(matrix_to_read)) error stop 'error: matrix_to_read not found'
    
        !size the array:
        allocate(imat(n_rows,n_cols))
    
        !grab each column of the matrix_to_read:
        ! [we need a json_core for this so we can use json_value_get_by_index]
        do i=1,n_cols
            call core%get_child(matrix_to_read,i,child)
            if (.not. associated(child)) error stop 'error: column not found'
            call core%get(child,ivec) !get the vector of integers (column of the matrix)
            if (.not. allocated(ivec)) error stop 'error: could not get integer column'
            if (size(ivec)/=n_rows) error stop 'error: column is wrong size'
            imat(:,i) = ivec
            deallocate(ivec)
            nullify(child)
        end do
        nullify(matrix_to_read)
        imat = transpose(imat)
    end function read_matrix_from_json

end module helper_functions