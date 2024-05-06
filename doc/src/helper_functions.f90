module helper_functions
    use json_module
    use, intrinsic :: iso_c_binding, only: cdp => c_double, cint => c_int 
    ! use funtional
    implicit none
    private

    interface
        integer function c_chdir(path) bind(C,name="chdir")
            use iso_c_binding
            character(kind=c_char) :: path(*)
        end function
    end interface
  
    public :: append_int, append_real64, point_table, print_matrix, &
                print_column_vector, read_matrix_from_json, &
                check_working_environment, read_data_from_json

    contains

    function point_table(poly_ord) result(p_table)
        integer(cint), intent(in) :: poly_ord
        real(cdp), allocatable :: p_table(:)
        real(cdp) :: res
        integer(cint) :: j
        allocate(p_table(poly_ord + 1))
        do j = 1, (poly_ord + 1)
            res = -1._cdp + (j - 1._cdp) * 2._cdp / poly_ord
            p_table(j) = res
        end do
    end function point_table


    subroutine append_int(vec, val)
        !***********************************************************************
        !> \brief Appends val in vec if not already present
        !> \date 05 2020
        !***********************************************************************
        integer(cint), allocatable, intent(inout)    :: vec(:)
        integer(cint), intent(in)                    :: val
        vec = [vec, val]
    end subroutine append_int

    subroutine append_real64(vec, val)
        !***********************************************************************
        !> \brief Appends val in vec if not already present
        !> \date 05 2020
        !***********************************************************************
        real(cdp), allocatable, intent(inout)    :: vec(:)
        real(cdp), intent(in)                    :: val
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
        integer(cint), intent(in) :: n
        integer(cint), intent(in) :: m
        real(cdp), intent(in) :: a(m,n)
        integer(cint) :: i,j
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
        integer(cint), intent(in) :: m
        real(cdp), intent(in) :: a(m)
        integer(cint) :: i
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
        real(cdp), allocatable :: ivec(:)
        real(cdp), allocatable :: imat(:,:)
        integer(cint) :: i,n_cols,n_rows,var_type
        logical :: found
        type(json_value),pointer :: matrix_to_read, child
        type(json_core) :: core
    
        !load the file and print it to console:
        call json%load_file(path)
        if (json%failed()) error stop 'error loading file'
    
        !get number of rows and columns
        !assuming data stored by column
        !assuming each column has the same number of elements,
        !and is the same data type (integer(cint) in this case):
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
            call core%get(child,ivec) !get the vector of integer(cint)s (column of the matrix)
            if (.not. allocated(ivec)) error stop 'error: could not get integer(cint) column'
            if (size(ivec)/=n_rows) error stop 'error: column is wrong size'
            imat(:,i) = ivec
            deallocate(ivec)
            nullify(child)
        end do
        nullify(matrix_to_read)
        imat = transpose(imat)
        call json%destroy()
    end function read_matrix_from_json


    subroutine read_data_from_json(polygon_order, number_of_elements, dofs, number_of_nodes, shape_fct_type , path)
        character(len=*), intent(in) :: path
        integer(cint), intent(inout) :: polygon_order
        integer(cint), intent(inout) :: number_of_elements
        integer(cint), intent(inout) :: dofs
        integer(cint), intent(inout) :: number_of_nodes
        character(len=60), intent(out) ::shape_fct_type
        type(json_file)            :: json
        logical                    :: is_found
        
        call json%initialize()

        ! Load the file.
        call json%load_file(path); if (json%failed()) stop

        ! Read in the data.
        json_block: block
		    !call json%get("t0", t0, is_found); if (.not. is_found) exit json_block
            call json%get("polyOrd", polygon_order, is_found); if (.not. is_found) exit json_block
            call json%get("ielem", number_of_elements, is_found); if (.not. is_found) exit json_block
            call json%get("dim", dofs, is_found); if (.not. is_found) exit json_block
            call json%get("nodedim", number_of_nodes, is_found); if (.not. is_found) exit json_block
            !call json%get("shapeFct", shape_fct_type, is_found); if (.not. is_found) exit json_block
		    !call json%get("elementsM", elementsM, is_found); if (.not. is_found) exit json_block
        end block json_block

    
        call json%destroy()
      end subroutine read_data_from_json

    subroutine chdir(path, err)
        use iso_c_binding
        character(*) :: path
        integer(cint), optional, intent(out) :: err
        integer(cint) :: loc_err
  
        loc_err =  c_chdir(path//c_null_char)
  
        if (present(err)) err = loc_err
    end subroutine

    subroutine to_lower(string)
        character(len=*), intent(inout) :: string
        integer(cint) :: i
    
        do i = 1, len_trim(string)
          if (iachar(string(i:i)) >= iachar('A') .and. iachar(string(i:i)) <= iachar('Z')) then
            string(i:i) = char(iachar(string(i:i)) + 32)
          end if
        end do
    
      end subroutine to_lower

    subroutine check_working_environment()
        implicit none
        character(len=500) :: cwd
        character(len=10) :: last_nine
        character(len=10) :: check_dir = "/sbfem/bin"
        integer(cint) :: str_len, start_pos
        call get_environment_variable('PWD',cwd)
        call to_lower(cwd)
        print *, "The current working directory is: ",trim(cwd)
        str_len = len_trim(cwd)  ! Get length of string without trailing spaces
        start_pos = max(str_len -9, 1)  ! Calculate starting position of last three characters
        last_nine = cwd(start_pos:str_len)
        if (check_dir == last_nine) then
            print*, "! You are in the currect working directory !"
        else if ("tran/sbfem" == last_nine) then
            print*, "! you are NOT in the currect working directory, but " // &
                    "i changes it for you to /bin:"
            call chdir("./bin")
            call execute_command_line("pwd")
        print*, "";
        else
            print*, "! you are NOT in the currect working directory !"
        end if
        print*, "----------------------------------------------------------";

    end subroutine check_working_environment

end module helper_functions