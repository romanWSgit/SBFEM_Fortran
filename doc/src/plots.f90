module plots
    use iso_fortran_env
    use functional, only: arange
    use math, only: lagrange, lagrange_diff
    use helper_functions, only: point_table
    use sbfem_functions, only: r_hat, r
    use SbfemClass
    implicit none
    private
  
    public :: plot_shape_functions, plot_superElement

    contains


    subroutine plot_shape_functions(poly_ord)
        !***********************************************************************
        
        !***********************************************************************
        implicit none
        integer, intent(in) :: poly_ord ! polynominal order
        integer :: i,j
        real(real64), allocatable :: xm(:)
        integer :: io ! output 1
        integer :: grid_size = 1000
        real(real64), allocatable :: plot_data_points(:)          
        real(real64) :: lower_bound = -1.0  ! lower bound for lagrange shape functions (standard: -1.0)
        real(real64) :: upper_bound         ! upper bound for lagrange shape functions (standard: 1.0)
        upper_bound = 1._real64 + 2._real64/grid_size
        plot_data_points = arange(lower_bound, upper_bound, 2._real64/grid_size)
        xm  = point_table(poly_ord)         ! (array of root points for lagrangian polynominals)  
    
        !open file to store points for plotting
        open(newunit=io, file="../plots/data/lagrangeShapeFct.dat", status="replace")
        ! plot lagrange shape functions
        do i=1, grid_size + 1
            write(io,*) plot_data_points(i), (lagrange(plot_data_points(i), j, xm), j = 1, size(xm))     
        end do
        ! close file
        close(io)

        !open file to store points for plotting
        open(newunit=io, file="../plots/data/lagrangeShapeFctDeriv.dat", status="replace")
        ! plot lagrange shape functions
        do i=1, grid_size + 1
            write(io,*) plot_data_points(i), (lagrange_diff(plot_data_points(i), j, xm), j = 1, size(xm))     
        end do
        ! close file
        close(io)
        
       
        
        ! create plots
        ! ! make clean
        !call execute_command_line("( cd plots && sh display_plots.sh )")
    end subroutine plot_shape_functions

    subroutine plot_superElement(sE)
        !***********************************************************************
        
        !***********************************************************************
        implicit none
        type(superElement), intent(in) :: sE
        integer :: i,j
        integer :: i_elem, i_nodes, u_bund
        integer, allocatable :: elem_shape(:), nodes_shape(:)
        integer :: io1, io2, io3 ! output 1
        ! integer :: grid_size = 1000
        ! real(real64), allocatable :: plot_data_points(:)          
        ! real(real64) :: lower_bound = -1.0  ! lower bound for lagrange shape functions (standard: -1.0)
        ! real(real64) :: upper_bound         ! upper bound for lagrange shape functions (standard: 1.0)
        ! upper_bound = 1._real64 + 2._real64/grid_size
        ! plot_data_points = arange(lower_bound, upper_bound, 2._real64/grid_size)
        ! xm  = point_table(poly_ord)         ! (array of root points for lagrangian polynominals)  
        elem_shape = shape(sE%elements_M)
        nodes_shape= shape(sE%nodes)
        i_elem = elem_shape(1)
        i_nodes = nodes_shape(1)
        u_bund = ubound(sE%elements_M,dim=2)
        ! open file to store points for plotting
        open(newunit=io1, file="../plots/data/polygon2.dat", status="replace")
        open(newunit=io2, file="../plots/data/polygon.dat", status="replace")
        open(newunit=io3, file="../plots/data/nodes.dat", status="replace")
        ! plot polygon data
        do i=1, i_elem
            write(io1,*) sE%elements_M(i,1:2)
            write(io1,*) sE%elements_M(i,u_bund - 1:u_bund)
            write(io1,*) 0.0_real64, 0.0_real64
            write(io1,*) sE%elements_M(i,1:2) 
            write(io1,*) ""
            write(io1,*) ""
            write(io2,*) sE%elements_M(i,1:2), sE%elements_M(i,u_bund - 1:u_bund), &
                        0.0_real64, 0.0_real64
        end do
        do i=1, i_nodes
            write(io3,*) sE%nodes(i,:)
        end do
        ! close file
        close(io1)
        close(io2)
        close(io3)
        
        
       
        
        ! create plots
        !  make clean
        call execute_command_line("sh ../plots/display_plots.sh")
    end subroutine plot_superElement


   

end module plots