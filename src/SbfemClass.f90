module SbfemClass
use iso_fortran_env
    implicit none
    private ! everything is hidden



        type, public :: superElement
            integer :: polygon_order
            integer :: number_of_elements
            real(real64), allocatable :: nodes(:,:)
            real(real64), allocatable :: nodes_C(:,:)
            real(real64), allocatable :: elements_M(:,:)
            real(real64), allocatable :: elements_MC(:,:)
        contains
            procedure :: print_super_element => print_sE
            ! procedure :: read_super_element_from_json => read_sE_from_json
            !final :: destructor
        end type superElement

    ! interface superElement
    !     module procedure Constructor
    ! end interface superElement

    contains

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

    subroutine print_sE(this)
        class(superElement), intent(in) ::  this
        print* , this%polygon_order
        print* , this%number_of_elements
    end subroutine

    ! subroutine read_sE_from_json(this)
    !     class(superElement), intent(in) ::  this
        
    ! end subroutine

end module SbfemClass