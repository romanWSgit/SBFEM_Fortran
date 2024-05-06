program example
		
	use iso_fortran_env
	
    use :: json_module, rk => json_rk
    implicit none

    !real(real64), allocatable  :: nodes(:,:), elementsM(:,:)
	real(kind = rk), allocatable :: nodes(:,:)
    type(json_file)            :: json
    logical                    :: is_found

    ! Initialise the json_file object.
    call json%initialize()

    ! Load the file.
    call json%load_file('/Users/roman_w_s/Desktop/SBFEM_DATA/rect_1.json'); if (json%failed()) stop

    ! Read in the data.
    json_block: block
		!call json%get("t0", t0, is_found); if (.not. is_found) exit json_block
        call json%get("nodes", nodes, is_found); if (.not. is_found) exit json_block
		!call json%get("elementsM", elementsM, is_found); if (.not. is_found) exit json_block
    end block json_block
	print *, nodes
    ! Output values.
    if (is_found) then
    	print *, nodes(1)
! 		print *, elementsM
    end if

    ! Clean up.
    call json%destroy()
end program example
