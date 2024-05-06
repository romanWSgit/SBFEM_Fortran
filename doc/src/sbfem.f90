program sbfem
    !! This is an simple program
    use, intrinsic :: iso_c_binding, only: cdp => c_double, cint => c_int 
    use functional, only: arange
    use plots, only: plot_shape_functions, plot_superElement
    use helper_functions, only: print_matrix, print_column_vector, &
                                read_matrix_from_json, check_working_environment, &
                                read_data_from_json
    use sbfem_functions, only: n_vec_f, n_mat_f, r_hat, r, j_hat_mat, j_mat
 

    !use math, only: lagrange, lagrange_diff
    use SbfemClass
   
    implicit none

    integer(cint) :: poly_ord = 1
    type(superElement) :: sE
    Character(len = 50) :: path
    Character(len = 20) :: object1
    integer(cint), allocatable :: shape_elementsM(:)
    integer(cint), allocatable :: shape_nodes(:)
    !real(cdp),allocatable :: elements_M(:,:)
    
    ! changing the working directory to a the bin
    call check_working_environment()
    


 

    path = '/Users/roman_w_s/Desktop/SBFEM_DATA/rect_1.json'
    !object1 = "elementsM"
    sE = superElement(path)
    ! read_data_from_json(sE%polygon_order, sE%number_of_elements, & 
    !                     sE%dof, sE%number_of_nodes, path)

    ! call plot_shape_functions(poly_ord)
    ! call sE%print_super_element()
    print*, n_vec_f(0.5_cdp, poly_ord,.true.)
   
    
    

    ! sE%nodes = read_matrix_from_json( "nodes", path)
    ! sE%nodes_C= read_matrix_from_json( "nodesC", path)
    ! sE%elements_M = read_matrix_from_json( "elementsM", path)
    ! sE%elements_MC = read_matrix_from_json( "elementsMC", path)
    
    print* , "Dimensions of elementsM: ", shape(sE%elements_M)
    print* , "Dimensions of elementsM 1:", shape(sE%elements_M(1,:))
    
    shape_elementsM = shape(sE%elements_M)
    call print_matrix(sE%elements_M, shape_elementsM(1),shape_elementsM(2))
    shape_nodes = shape(sE%nodes)
    call print_matrix(sE%nodes, shape_nodes(1),shape_nodes(2))
    call print_column_vector(sE%elements_M(1,:),4)
    call print_column_vector(r(eta = -1._cdp, &
        poly_ord = poly_ord, coord_vec = sE%elements_M(1,:) ), 2)
    call print_column_vector(r_hat(xi = 0.1_cdp, eta = -1._cdp, &
        poly_ord = poly_ord, coord_vec = sE%elements_M(1,:) ), 2)
    
    ! call plot_superElement(sE)

    
    call sE%print_super_element()

    ! print*, j_hat_mat(xi, eta, coord_vec, poly_ord, shape_function_type, centre) 
    print*, sE%elements_M(1,:)
    print*, j_hat_mat(0.1_cdp, 0.1_cdp, sE%elements_M(1,:), sE%polygon_order, sE%shape_function_type, [0.0_cdp,0.0_cdp]) 
    
    print*, sE%shape_function_type
    
end program sbfem

