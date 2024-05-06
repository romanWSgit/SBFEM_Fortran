module SbfemClass
use iso_fortran_env
use helper_functions, only: print_matrix, print_column_vector, &
                                read_matrix_from_json, check_working_environment, &
                                read_data_from_json
    implicit none
    private ! everything is hidden

        type, public :: superElement
            integer :: polygon_order
            integer :: number_of_elements
            integer :: number_of_nodes
            integer :: dof
            character(len=:), allocatable :: shape_function_type
            real(real64), allocatable :: nodes(:,:)
            real(real64), allocatable :: nodes_C(:,:)
            real(real64), allocatable :: elements_M(:,:)
            real(real64), allocatable :: elements_MC(:,:)
            
        contains
            procedure, pass(this) :: print_super_element
            ! procedure :: read_super_element_from_json => read_sE_from_json
            !final :: destructor
        end type superElement

    interface superElement
        module procedure :: superElement_json_constructor
    end interface superElement
    
    contains

    type(superElement) function superElement_json_constructor(path) result(res) 
        Character(*), intent(in) :: path

        call read_data_from_json(res%polygon_order, &
            res%number_of_elements,res%dof, res%number_of_nodes, res%shape_function_type, &
            path)
        res%nodes = read_matrix_from_json("nodes", path)
        res%nodes_C = read_matrix_from_json("nodesC", path)
        res%elements_M = read_matrix_from_json("elementsM", path)
        res%elements_MC = read_matrix_from_json("elementsMC", path)
        
        
    end function superElement_json_constructor

    ! function Constructor() Result(superElement)
    !     type(superElement) :: sE
   
    !     !initialize variables directly
    !     sE%x=...
    !     ! or through method calls
    !     call Timer%setTime(now)
    
    ! end function Constructor


    ! subroutine destructor(this)
    !     class(superElement) :: this
    !     ! Do whatever needs doing in the destructor
    ! end subroutine destructor

    subroutine print_super_element(this)
        class(superElement), intent(in) ::  this
        print*, "sE: Polygon Order: ",this%polygon_order
        print*, "sE: Number of Elements: ",this%number_of_elements
        print*, "sE: Shape Function Type: ",this%shape_function_type
    end subroutine print_super_element

    ! subroutine read_sE_from_json(this)
    !     class(superElement), intent(in) ::  this
        
    ! end subroutine

end module SbfemClass