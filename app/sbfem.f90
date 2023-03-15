program sbfem
    !! This is our program
    use iso_fortran_env
    use functional, only: arange
    use plots, only: plot_shape_functions, plot_superElement
    use helper_functions, only: print_matrix, print_column_vector, read_matrix_from_json
    use sbfem_functions, only: n_vec_f, n_mat_f, r_hat, r
    !use math, only: lagrange, lagrange_diff
    use SbfemClass
   
    implicit none
    integer :: poly_ord = 1
    type(superElement) :: sE
    Character(len = 50) :: path
    Character(len = 20) :: object1
    integer, allocatable :: shape_elementsM(:)
    integer, allocatable :: shape_nodes(:)
    !real(real64),allocatable :: elements_M(:,:)

    path = '/Users/roman_w_s/Desktop/SBFEM_DATA/rect_1.json'
    !object1 = "elementsM"
    sE = superElement(2,10)
    print*, sE%polygon_order
    print*, sE%number_of_elements
    call plot_shape_functions(poly_ord)
    call sE%print_super_element()
    print*, n_vec_f(0.5_real64, poly_ord,.true.)
    
    sE%nodes = read_matrix_from_json( "nodes", path)
    sE%nodes_C= read_matrix_from_json( "nodesC", path)
    sE%elements_M = read_matrix_from_json( "elementsM", path)
    sE%elements_MC = read_matrix_from_json( "elementsMC", path)
    print* , "Dimensions of elementsM: ", shape(sE%elements_M)
    print* , "Dimensions of elementsM 1:", shape(sE%elements_M(1,:))
    
    shape_elementsM = shape(sE%elements_M)
    call print_matrix(sE%elements_M, shape_elementsM(1),shape_elementsM(2))
    shape_nodes = shape(sE%nodes)
    call print_matrix(sE%nodes, shape_nodes(1),shape_nodes(2))
    call print_column_vector(sE%elements_M(1,:),4)
    call print_column_vector(r(eta = -1._real64, &
        poly_ord = poly_ord, coord_vec = sE%elements_M(1,:) ), 2)
    call print_column_vector(r_hat(xi = 0.1_real64, eta = -1._real64, &
        poly_ord = poly_ord, coord_vec = sE%elements_M(1,:) ), 2)
    
    call plot_superElement(sE)

    

        
   
end program sbfem

