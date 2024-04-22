open OUnit2
open Final_project

let () =
  print_string "Adding 1.0 and 2.0: ";
  print_endline (string_of_float (Basicops.add 1. 2.))

let () =
  print_string "Subtracting 3.0 from 5.0: ";
  print_endline (string_of_float (Basicops.subtract 5. 3.))

let () =
  print_string "Multiplying 3.0 and 4.0: ";
  print_endline (string_of_float (Basicops.multiply 3. 4.))

let () =
  print_string "Dividing 8.0 by 2.0: ";
  print_endline (string_of_float (Basicops.divide 8. 2.))

let () = print_endline ""
let matrix_a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
let matrix_b = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]

let () =
  print_endline "Matrix A: ";
  print_string (Matrix.to_string matrix_a)

let () =
  print_endline "Matrix B: ";
  print_string (Matrix.to_string matrix_b)

let () =
  print_endline "Identity matrix 3: ";
  print_string (Matrix.to_string (Matrix.identity_matrix 3))

let () =
  print_endline "Adding matrix A and matrix B: ";
  print_string (Matrix.to_string (Matrix.add matrix_a matrix_b))

let () =
  print_endline "Subtracting matrix A and matrix B: ";
  print_string (Matrix.to_string (Matrix.subtract matrix_a matrix_b))

let () =
  print_endline "Transposing matrix A: ";
  print_string (Matrix.to_string (Matrix.transpose matrix_a))

let () =
  print_endline "Multiplying matrix A and matrix B: ";
  print_string (Matrix.to_string (Matrix.multiply matrix_a matrix_b))

let () =
  print_endline "Multiplying 4 and matrix B: ";
  print_string (Matrix.to_string (Matrix.scalar_multiply 4. matrix_b))

let () =
  print_endline "Determinant of matrix A: ";
  print_float (Matrix.determinant matrix_a);
  print_endline ""

let () =
  print_endline "Determinant of matrix B: ";
  print_float (Matrix.determinant matrix_b);
  print_endline ""

let () =
  print_endline "Determinant of identity matrix: ";
  print_float (Matrix.determinant (Matrix.identity_matrix 2));
  print_endline ""

let () =
  print_endline "Swap rows 0 and 1 of matrix A: ";
  print_string (Matrix.to_string (Matrix.swap_rows matrix_a 0 1))

let () =
  print_endline "Swap rows 0 and 1 of matrix B: ";
  print_string (Matrix.to_string (Matrix.swap_rows matrix_b 0 1))

let () =
  print_endline "Swap rows 1 and 2 of identity matrix 3: ";
  print_string
    (Matrix.to_string (Matrix.swap_rows (Matrix.identity_matrix 3) 2 1))

let () =
  print_endline "Swap columns 0 and 1 of matrix A: ";
  print_string (Matrix.to_string (Matrix.swap_cols matrix_a 0 1))

let () =
  print_endline "Swap columns 0 and 1 of matrix B: ";
  print_string (Matrix.to_string (Matrix.swap_cols matrix_b 0 1))

let () =
  print_endline "Swap columns 1 and 2 of identity matrix 3: ";
  print_string
    (Matrix.to_string (Matrix.swap_cols (Matrix.identity_matrix 3) 2 1))

let () =
  print_endline "Row reduce matrix A: ";
  print_string (Matrix.to_string (Matrix.row_reduce matrix_a))

let () =
  print_endline "Row reduce matrix B: ";
  print_string (Matrix.to_string (Matrix.row_reduce matrix_b))

let () =
  print_endline "Row reduce identity matrix 3: ";
  print_string (Matrix.to_string (Matrix.row_reduce (Matrix.identity_matrix 3)))

let () =
  print_endline "Rank of matrix A: ";
  print_int (Matrix.rank matrix_a);
  print_endline ""

let () =
  print_endline "Rank of matrix B: ";
  print_int (Matrix.rank matrix_b);
  print_endline ""

let () =
  print_endline "Rank of identity matrix 3: ";
  print_int (Matrix.rank (Matrix.identity_matrix 3));
  print_endline ""

let () = print_endline ""

let () =
  print_string "Calculating sin 45 degrees: ";
  print_endline (string_of_float (Trig.sin 45))

let () =
  print_string "Calculating cos 30 degrees: ";
  print_endline (string_of_float (Trig.cos 30))

let () =
  print_string "Calculating tan 180 degrees: ";
  print_endline (string_of_float (Trig.tan 180))

let () = print_endline ""

let () =
  print_string "Calculating the mean of [1.0; 5.0; 4.0; 10.0]: ";
  print_endline (string_of_float (Probstats.mean [ 1.0; 5.0; 4.0; 10.0 ]))

let () =
  print_string "Calculating the median of [1.0; 5.0; 4.0; 10.0]: ";
  print_endline (string_of_float (Probstats.median [ 1.0; 5.0; 4.0; 10.0 ]))

let () =
  print_string "Calculating the variance of [1.0; 5.0; 4.0; 10.0]: ";
  print_endline (string_of_float (Probstats.variance [ 1.0; 5.0; 4.0; 10.0 ]))

let () = print_endline ""

let () =
  print_string "Pi.";
  print_endline (string_of_float Poly.pi)

let () =
  print_string "E.";
  print_endline (string_of_float Poly.e)

let () =
  print_string "Convert radian value [Poly.pi] to degree";
  print_endline (Poly.pi |> Poly.rad_to_deg |> string_of_float)

let () =
  print_string "Convert degree value [360] to radian.";
  print_endline (360. |> Poly.deg_to_rad |> string_of_float)

let () =
  print_string "Evaluate polynomial [2, 3, 4, 2] at a given value of\n\n   2.0.";
  print_endline (Poly.eval 2. [ 2.; 3.; 4.; 2. ] |> string_of_float)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] once.";
  print_endline (Poly.deri_once [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] 2 times.";
  print_endline (Poly.deri 2 [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string
    "Integral the polynomial [2, 3, 4, 2] once, with\n\n   constant = 2 added.";
  print_endline
    (Poly.inte_once 2. [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string "Add two polynomials [2, 3, 4, 2] and [3, 0, 1]";
  print_endline
    (Poly.add [ 2.; 3.; 4.; 2. ] [ 3.; 0.; 1. ] |> Poly.string_of_float_list)

let () =
  print_string "Calculate the degree of polynomial [0, 2, 3, 4, 2]";
  print_endline (Poly.degree [ 0.; 2.; 3.; 4.; 2. ] |> string_of_int)

let () =
  print_string "Multiply two polynomials [2, 3] and [4, 2]";
  print_endline
    (Poly.multiply [ 2.; 3. ] [ 4.; 2. ] |> Poly.string_of_float_list)

let tests =
  "test suite"
  >::: [
         ("Add" >:: fun _ -> assert_equal (Basicops.add 1. 2.) 3.);
         ("Subtract" >:: fun _ -> assert_equal (Basicops.subtract 5. 3.) 2.);
         ("Multiply" >:: fun _ -> assert_equal (Basicops.multiply 3. 4.) 12.);
         ("Divide" >:: fun _ -> assert_equal (Basicops.divide 8. 2.) 4.);
         ( "Matrix A" >:: fun _ ->
           assert_equal (Matrix.to_string matrix_a) "1. 2. \n3. 4. \n" );
         ( "Matrix B" >:: fun _ ->
           assert_equal (Matrix.to_string matrix_b) "5. 6. \n7. 8. \n" );
         ( "Identity matrix 3" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.identity_matrix 3))
             "1. 0. 0. \n0. 1. 0. \n0. 0. 1. \n" );
         ( "Adding matrix A and matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.add matrix_a matrix_b))
             "6. 8. \n10. 12. \n" );
         ( "Subtracting matrix A and matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.subtract matrix_a matrix_b))
             "-4. -4. \n-4. -4. \n" );
         ( "Transposing matrix A" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.transpose matrix_a))
             "1. 3. \n2. 4. \n" );
         ( "Transposing matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.transpose matrix_b))
             "5. 7. \n6. 8. \n" );
         ( "Multiplying matrix A and matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.multiply matrix_a matrix_b))
             "19. 22. \n43. 50. \n" );
         ( "Multiplying 4 and matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.scalar_multiply 4. matrix_b))
             "20. 24. \n28. 32. \n" );
         ( "Determinant of matrix A" >:: fun _ ->
           assert_equal (Matrix.determinant matrix_a) (-2.) );
         ( "Determinant of matrix B" >:: fun _ ->
           assert_equal (Matrix.determinant matrix_a) (-2.) );
         ( "Determinant of identity matrix" >:: fun _ ->
           assert_equal (Matrix.determinant (Matrix.identity_matrix 2)) 1. );
         ( "Swap rows 0 and 1 of matrix A" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.swap_rows matrix_a 0 1))
             "3. 4. \n1. 2. \n" );
         ( "Swap rows 0 and 1 of matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.swap_rows matrix_b 0 1))
             "7. 8. \n5. 6. \n" );
         ( "Swap rows 0 and 1 of identity matrix 3" >:: fun _ ->
           assert_equal
             (Matrix.to_string
                (Matrix.swap_rows (Matrix.identity_matrix 3) 2 1))
             "1. 0. 0. \n0. 0. 1. \n0. 1. 0. \n" );
         ( "Swap columns 0 and 1 of matrix A" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.swap_cols matrix_a 0 1))
             "2. 1. \n4. 3. \n" );
         ( "Swap columns 0 and 1 of matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.swap_cols matrix_b 0 1))
             "6. 5. \n8. 7. \n" );
         ( "Swap columns 0 and 1 of identity matrix 3" >:: fun _ ->
           assert_equal
             (Matrix.to_string
                (Matrix.swap_cols (Matrix.identity_matrix 3) 2 1))
             "1. 0. 0. \n0. 0. 1. \n0. 1. 0. \n" );
         ( "Row reduce matrix A" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.row_reduce matrix_a))
             "1. 0. \n-0. 1. \n" );
         ( "Row reduce matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.row_reduce matrix_b))
             "1. 0. \n-0. 1. \n" );
         ( "Row reduce identity matrix 3" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.row_reduce (Matrix.identity_matrix 3)))
             "1. 0. 0. \n0. 1. 0. \n0. 0. 1. \n" );
         ("Rank of matrix A" >:: fun _ -> assert_equal (Matrix.rank matrix_a) 2);
         ("Rank of matrix B" >:: fun _ -> assert_equal (Matrix.rank matrix_b) 2);
         ( "Rank of identity matrix 3" >:: fun _ ->
           assert_equal (Matrix.rank (Matrix.identity_matrix 3)) 3 );
         ("sin45" >:: fun _ -> assert_equal (Trig.sin 45) 0.707106781186547462);
         ("sin30" >:: fun _ -> assert_equal (Trig.sin 30) 0.499999999999999944);
         ("sin60" >:: fun _ -> assert_equal (Trig.sin 60) 0.866025403784438486);
         ("cos45" >:: fun _ -> assert_equal (Trig.cos 45) 0.707106781186547462);
         ("cos30" >:: fun _ -> assert_equal (Trig.cos 30) 0.866025403784438597);
         ("cos60" >:: fun _ -> assert_equal (Trig.cos 60) 0.500000000000000111);
         ("tan45" >:: fun _ -> assert_equal (Trig.tan 45) 1.);
         ("tan30" >:: fun _ -> assert_equal (Trig.tan 30) 0.577350269189625731);
         ("tan60" >:: fun _ -> assert_equal (Trig.tan 60) 1.73205080756887653);
       ]

let _ = run_test_tt_main tests
