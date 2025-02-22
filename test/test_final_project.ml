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

let () =
  print_string "2 to the power of 3: ";
  print_endline (string_of_float (Basicops.pow 2. 3))

let () =
  print_string "Triple root of 8: ";
  print_endline (string_of_float (Basicops.nth_root 8. 3))

let () =
  print_string "Absolute value of -5: ";
  print_endline (string_of_float (Basicops.abs (-5.)))

let () =
  print_string "Gcd of 4 and 6: ";
  print_endline (string_of_int (Basicops.gcd 4 6))

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
  print_endline "Adding matrix A and 35: ";
  print_string (Matrix.to_string (Matrix.scalar_add matrix_a 35.))

let () =
  print_endline "Subtracting matrix B by 10: ";
  print_string (Matrix.to_string (Matrix.scalar_subtract matrix_b 10.))

let () =
  print_endline "Multiplying matrix B and 4: ";
  print_string (Matrix.to_string (Matrix.scalar_multiply matrix_b 4.))

let () =
  print_endline "Dividing matrix A by 6: ";
  print_string (Matrix.to_string (Matrix.scalar_divide matrix_a 6.))

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

let () =
  print_endline "Calculator string of matrix A: ";
  print_endline (Matrix.to_calculator_string matrix_a)

let () =
  print_endline "Calculator string of matrix B: ";
  print_endline (Matrix.to_calculator_string matrix_b)

let () =
  print_string
    (Matrix.to_string (Matrix.from_calculator_string "[[1.,2.],[3.,4.]]"))

let () =
  print_string
    (Matrix.to_string (Matrix.from_calculator_string "[[5.,6.],[7.,8.]]"))

let () = print_endline ""

let () =
  print_string "Calculating sin 45 degrees: ";
  print_endline (string_of_float (Trig.sin 45.))

let () =
  print_string "Calculating sin 35.5 degrees: ";
  print_endline (string_of_float (Trig.sin 35.5))

let () =
  print_string "Calculating sin 180 degrees: ";
  print_endline (string_of_float (Trig.sin 180.))

let () =
  print_string "Calculating cos 30 degrees: ";
  print_endline (string_of_float (Trig.cos 30.))

let () =
  print_string "Calculating cos 29.4 degrees: ";
  print_endline (string_of_float (Trig.cos 29.4))

let () =
  print_string "Calculating cos 90 degrees: ";
  print_endline (string_of_float (Trig.cos 90.))

let () =
  print_string "Calculating tan 180 degrees: ";
  print_endline (string_of_float (Trig.tan 180.))

let () =
  print_string "Calculating tan 7 degrees: ";
  print_endline (string_of_float (Trig.tan 7.))

let () =
  print_string "Calculating tan 45 degrees: ";
  print_endline (string_of_float (Trig.tan 45.))

let () =
  print_string "Calculating cosecant 45 degrees: ";
  print_endline (string_of_float (Trig.csc 45.))

let () =
  print_string "Calculating cosecant 30 degrees: ";
  print_endline (string_of_float (Trig.csc 30.))

let () =
  print_string "Calculating cosecant 60 degrees: ";
  print_endline (string_of_float (Trig.csc 60.))

let () =
  print_string "Calculating secant 45 degrees: ";
  print_endline (string_of_float (Trig.sec 45.))

let () =
  print_string "Calculating secant 30 degrees: ";
  print_endline (string_of_float (Trig.sec 30.))

let () =
  print_string "Calculating secant 60 degrees: ";
  print_endline (string_of_float (Trig.sec 60.))

let () =
  print_string "Calculating cotangent 45 degrees: ";
  print_endline (string_of_float (Trig.cot 45.))

let () =
  print_string "Calculating cotangent 30 degrees: ";
  print_endline (string_of_float (Trig.cot 30.))

let () =
  print_string "Calculating cotangent 60 degrees: ";
  print_endline (string_of_float (Trig.cot 60.))

let () =
  print_string "Calculating arcsine 0.5: ";
  print_endline (string_of_float (Trig.arcsin 0.5))

let () =
  print_string "Calculating arcsine 0.8: ";
  print_endline (string_of_float (Trig.arcsin 0.8))

let () =
  print_string "Calculating arcsine 1: ";
  print_endline (string_of_float (Trig.arcsin 1.))

let () =
  print_string "Calculating arccosine 0.5: ";
  print_endline (string_of_float (Trig.arccos 0.5))

let () =
  print_string "Calculating arccosine 0.8: ";
  print_endline (string_of_float (Trig.arccos 0.8))

let () =
  print_string "Calculating arccosine 1.: ";
  print_endline (string_of_float (Trig.arccos 1.))

let () =
  print_string "Calculating arctangent 0.5: ";
  print_endline (string_of_float (Trig.arctan 0.5))

let () =
  print_string "Calculating arctangent 0.8: ";
  print_endline (string_of_float (Trig.arctan 0.8))

let () =
  print_string "Calculating arctangent 1.: ";
  print_endline (string_of_float (Trig.arctan 1.))

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
  print_string
    "Evaluate polynomial [2, 3, 4, 2] at a given value\n   of\n\n 2.0.";
  print_endline (Poly.eval 2. [ 2.; 3.; 4.; 2. ] |> string_of_float)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] once.";
  print_endline (Poly.deri_once [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] 2 times.";
  print_endline (Poly.deri 2 [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string
    "Integral the polynomial [2, 3, 4, 2] once, with\n\n\n\
    \   constant = 2 added.";
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

let () =
  print_string "Convert polynomial [1.0; -3.5; 2.0] to readable\n   string: ";
  print_endline (Poly.poly_to_string [ 1.0; -3.5; 2.0 ])

let () =
  print_string "Compose two polynomials [2.0; 3.0; 2.0] and [0.0;\n   2.0]: ";
  print_endline
    (Poly.compose_polynomials [ 2.0; 3.0; 2.0 ] [ 0.0; 2.0 ]
    |> Poly.string_of_float_list)

let () =
  print_string
    "Convert polynomial [3.0; -2.0; 0.0; 1.0] to readable\n   string: ";
  print_endline (Poly.poly_to_string [ 3.0; -2.0; 0.0; 1.0 ])

let () =
  print_string "Convert polynomial [0.0; 0.0; 5.0] to readable string:\n   ";
  print_endline (Poly.poly_to_string [ 0.0; 0.0; 5.0 ])

let () =
  print_string
    "Convert polynomial [1.0; 0.0; -1.0; 0.0] to readable\n   string: ";
  print_endline (Poly.poly_to_string [ 1.0; 0.0; -1.0; 0.0 ])

let () =
  print_string "Calculate the mean of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline (Probstats.mean [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the median of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline (Probstats.median [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the variance of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline
    (Probstats.variance [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the standard deviation of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline (Probstats.sd [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the 0.75 quantile of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline
    (Probstats.quantile 0.75 [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the range of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline (Probstats.range [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the mode of [1.0; 2.0; 2.0; 3.0; 4.0]: ";
  print_endline (Probstats.mode [ 1.0; 2.0; 2.0; 3.0; 4.0 ] |> string_of_float)

let () =
  print_string "Calculate the skewness of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline
    (Probstats.skewness [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string "Calculate the z-score of 3.0 in [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline
    (Probstats.z_score 3.0 [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string
    "Calculate the interquartile range of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline (Probstats.iqr [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string
    "Calculate the coefficient of variation of [1.0; 2.0; 3.0; 4.0; 5.0]: ";
  print_endline
    (Probstats.coefficient_of_variation [ 1.0; 2.0; 3.0; 4.0; 5.0 ]
    |> string_of_float)

let () =
  print_string
    "Calculate the correlation of [1.0; 2.0; 3.0] and [4.0; 5.0; 6.0]: ";
  print_endline
    (Probstats.correlation [ 1.0; 2.0; 3.0 ] [ 4.0; 5.0; 6.0 ]
    |> string_of_float)

let () =
  print_string
    "Calculate the cumulative distribution function of 3.0 in [1.0; 2.0; 3.0; \
     4.0; 5.0]: ";
  print_endline
    (Probstats.cdf 3.0 [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> string_of_float)

let () =
  print_string
    "Calculate the linear regression of xs = [1.0; 2.0; 3.0] and ys = [2.0; \
     4.0; 6.0]: ";
  let slope, intercept =
    Probstats.linear_regression [ 1.0; 2.0; 3.0 ] [ 2.0; 4.0; 6.0 ]
  in
  print_endline
    ("slope: " ^ string_of_float slope ^ ", intercept: "
   ^ string_of_float intercept)

(* Define a function to check approximate equality of floats *)
let approx_equal a b tolerance = abs_float (a -. b) < tolerance

let tests =
  "test suite"
  >::: [
         ("Add" >:: fun _ -> assert_equal (Basicops.add 1. 2.) 3.);
         ("Subtract" >:: fun _ -> assert_equal (Basicops.subtract 5. 3.) 2.);
         ("Multiply" >:: fun _ -> assert_equal (Basicops.multiply 3. 4.) 12.);
         ("Divide" >:: fun _ -> assert_equal (Basicops.divide 8. 2.) 4.);
         ("Power" >:: fun _ -> assert_equal (Basicops.pow 2. 3) 8.);
         ( "Nth_root" >:: fun _ ->
           assert_equal (Basicops.nth_root 8. 3) 2.00000000001206191 );
         ("Absolute Value" >:: fun _ -> assert_equal (Basicops.abs (-5.)) 5.);
         ("Gcd" >:: fun _ -> assert_equal (Basicops.gcd 4 6) 2);
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
         ( "Adding matrix A and 35" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.scalar_add matrix_a 35.))
             "36. 37. \n38. 39. \n" );
         ( "Subtracting matrix B by 10" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.scalar_subtract matrix_b 10.))
             "-5. -4. \n-3. -2. \n" );
         ( "Multiplying matrix B and 4" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.scalar_multiply matrix_b 4.))
             "20. 24. \n28. 32. \n" );
         ( "Dividing\n   matrix A by 6" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.scalar_divide matrix_a 6.))
             "0.166666666667 0.333333333333 \n0.5 0.666666666667 \n" );
         ( "Determinant of matrix A" >:: fun _ ->
           assert_equal (Matrix.determinant matrix_a) (-2.) );
         ( "Determinant of matrix B" >:: fun _ ->
           assert_equal (Matrix.determinant matrix_a) (-2.) );
         ( "Determinant of\n   identity matrix" >:: fun _ ->
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
         ( "Row reduce identity\n   matrix 3" >:: fun _ ->
           assert_equal
             (Matrix.to_string (Matrix.row_reduce (Matrix.identity_matrix 3)))
             "1. 0. 0. \n0. 1. 0. \n0. 0. 1. \n" );
         ( "Rank of\n   matrix A" >:: fun _ ->
           assert_equal (Matrix.rank matrix_a) 2 );
         ( "Rank of\n   matrix B" >:: fun _ ->
           assert_equal (Matrix.rank matrix_b) 2 );
         ( "Rank of\n   identity matrix 3" >:: fun _ ->
           assert_equal (Matrix.rank (Matrix.identity_matrix 3)) 3 );
         ( "Calculator string of matrix A" >:: fun _ ->
           assert_equal
             (Matrix.to_calculator_string matrix_a)
             "[[1.,2.],[3.,4.]]" );
         ( "Calculator string of matrix B" >:: fun _ ->
           assert_equal
             (Matrix.to_calculator_string matrix_b)
             "[[5.,6.],[7.,8.]]" );
         ( "Obtain matrix A from calculator string [[1.,2.],[3.,4.]]"
         >:: fun _ ->
           assert_equal
             (Matrix.to_string
                (Matrix.from_calculator_string "[[1.,2.],[3.,4.]]"))
             (Matrix.to_string matrix_a) );
         ( "Obtain matrix B from calculator string [[5.,6.],[7.,8.]]"
         >:: fun _ ->
           assert_equal
             (Matrix.to_string
                (Matrix.from_calculator_string "[[5.,6.],[7.,8.]]"))
             (Matrix.to_string matrix_b) );
         ("sin45" >:: fun _ -> assert_equal (Trig.sin 45.) 0.707106781186547462);
         ("sin30" >:: fun _ -> assert_equal (Trig.sin 30.) 0.499999999999999944);
         ("sin60" >:: fun _ -> assert_equal (Trig.sin 60.) 0.866025403784438486);
         ("cos45" >:: fun _ -> assert_equal (Trig.cos 45.) 0.707106781186547462);
         ("cos30" >:: fun _ -> assert_equal (Trig.cos 30.) 0.866025403784438597);
         ("cos60" >:: fun _ -> assert_equal (Trig.cos 60.) 0.500000000000000111);
         ("tan45" >:: fun _ -> assert_equal (Trig.tan 45.) 1.);
         ("tan30" >:: fun _ -> assert_equal (Trig.tan 30.) 0.577350269189625731);
         ("tan60" >:: fun _ -> assert_equal (Trig.tan 60.) 1.73205080756887653);
         ("csc45" >:: fun _ -> assert_equal (Trig.csc 45.) 1.41421356237309515);
         ("csc30" >:: fun _ -> assert_equal (Trig.csc 30.) 2.00000000000000044);
         ("csc60" >:: fun _ -> assert_equal (Trig.csc 60.) 1.15470053837925168);
         ("sec45" >:: fun _ -> assert_equal (Trig.sec 45.) 1.41421356237309515);
         ("sec30" >:: fun _ -> assert_equal (Trig.sec 30.) 1.15470053837925168);
         ("sec60" >:: fun _ -> assert_equal (Trig.sec 60.) 1.99999999999999956);
         ("cot45" >:: fun _ -> assert_equal (Trig.cot 45.) 1.);
         ("cot30" >:: fun _ -> assert_equal (Trig.cot 30.) 1.73205080756887742);
         ("cot60" >:: fun _ -> assert_equal (Trig.cot 60.) 0.577350269189626064);
         ( "arcsin 0.5" >:: fun _ ->
           assert_equal (Trig.arcsin 0.5) 30.0000000000000036 );
         ( "arcsin 0.1" >:: fun _ ->
           assert_equal (Trig.arcsin 0.1) 5.73917047726678664 );
         ("arcsin 1." >:: fun _ -> assert_equal (Trig.arcsin 1.) 90.);
         ( "arccos 0.5" >:: fun _ ->
           assert_equal (Trig.arccos 0.5) 59.9999999999999929 );
         ( "arccos 0.1" >:: fun _ ->
           assert_equal (Trig.arccos 0.1) 84.2608295227332178 );
         ("arccos 1." >:: fun _ -> assert_equal (Trig.arccos 1.) 0.);
         ( "arctan 0.5" >:: fun _ ->
           assert_equal (Trig.arctan 0.5) 26.5650511770779758 );
         ( "arctan 0.1" >:: fun _ ->
           assert_equal (Trig.arctan 0.1) 5.71059313749964215 );
         ( "arctan 1." >:: fun _ ->
           assert_equal (Trig.arctan 1.) 44.8567641317560444 );
         ( "Convert radians\n   to degrees" >:: fun _ ->
           assert_equal (Poly.rad_to_deg Poly.pi) 180. );
         ( "Convert degrees to radians" >:: fun _ ->
           assert (approx_equal (Poly.deg_to_rad 180.) Poly.pi 0.0001) );
         ( "Evaluate polynomial at x=2" >:: fun _ ->
           assert_equal (Poly.eval 2. [ 1.; -3.; 2. ]) 3. );
         (* Evaluates 1 - 3*2 + 2*2^2 *)
         ( "First derivative of a polynomial" >:: fun _ ->
           assert_equal (Poly.deri_once [ 1.; -3.; 2. ]) [ 2.; -3. ] );
         (* Derivative of 1 - 3x + 2x^2 is -3 + 4x *)
         ( "N-th derivative of a polynomial (n=2)" >:: fun _ ->
           assert_equal (Poly.deri 2 [ 1.; -3.; 2. ]) [ 2. ] );
         (* Second derivative of 1 - 3x + 2x^2 is 4 *)
         ( "Integral of a polynomial with constant c=1" >:: fun _ ->
           assert_equal (Poly.inte_once 1. [ 6.; 6.; 6. ]) [ 2.; 3.; 6.; 1. ] );
         (* Integral of 1 - 3x + 2x^2 dx *)
         ( "Add two polynomials" >:: fun _ ->
           assert_equal (Poly.add [ 1.; 3. ] [ 2.; -1.; 4. ]) [ 3.; 2.; 4. ] );
         (* Adds 1 + 3x and 2 - x + 4x^2 *)
         ( "Degree of a polynomial" >:: fun _ ->
           assert_equal (Poly.degree [ 0.; 0.; 1.; -3.; 2. ]) 2 );
         (* Degree of 0 + 0x + 1x^2 - 3x^3 + 2x^4 *)
         ( "Multiply two polynomials" >:: fun _ ->
           assert_equal
             (Poly.multiply [ 1.; 3. ] [ 2.; -1.; 4. ])
             [ 2.; 5.; 1.; 12. ] );
         ( "GCD of two polynomials" >:: fun _ ->
           assert_equal
             (Poly.gcd_polynomial [ 1.; -3.; 2. ] [ 1.; -1. ])
             [ -2. ] );
         ( "Compose two polynomials" >:: fun _ ->
           assert_equal
             (Poly.compose_polynomials [ 2.; 0.; 1. ] [ 0.; 1. ])
             [ 2.; 0.; 1.; 1. ] );
         (* Composes 2 + x^2 with x *)
         ( "Subtract two polynomials" >:: fun _ ->
           assert_equal (Poly.subtract [ 2.; 3.; 4. ] [ 1.; 2. ]) [ 1.; 1.; 4. ]
         );
         ( "Evaluate polynomial at multiple points" >:: fun _ ->
           assert_equal
             (Poly.eval_at_points [ 1.; -3.; 2. ] [ 0.; 1.; 2. ])
             [ 1.; 0.; 3. ] );
         ( "Convert polynomial to string" >:: fun _ ->
           assert_equal (Poly.poly_to_string [ 1.; -3.; 2. ]) "2. + -3.x + x^2"
         );
         ( "Mean" >:: fun _ ->
           assert_equal (Probstats.mean [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 3.0 );
         ( "Median" >:: fun _ ->
           assert_equal (Probstats.median [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 3.0 );
         ( "Variance" >:: fun _ ->
           assert_equal (Probstats.variance [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 2.0 );
         ( "Standard Deviation" >:: fun _ ->
           assert_equal (Probstats.sd [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) (sqrt 2.0) );
         ( "Quantile" >:: fun _ ->
           assert_equal
             (Probstats.quantile 0.75 [ 1.0; 2.0; 3.0; 4.0; 5.0 ])
             4.0 );
         ( "Range" >:: fun _ ->
           assert_equal (Probstats.range [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 4.0 );
         ( "Mode" >:: fun _ ->
           assert_equal (Probstats.mode [ 1.0; 2.0; 2.0; 3.0; 4.0 ]) 2.0 );
         ( "Skewness" >:: fun _ ->
           assert_equal (Probstats.skewness [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 0.0 );
         ( "Z-Score" >:: fun _ ->
           assert_equal (Probstats.z_score 3.0 [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 0.0
         );
         ( "Interquartile Range" >:: fun _ ->
           assert_equal (Probstats.iqr [ 1.0; 2.0; 3.0; 4.0; 5.0 ]) 2.0 );
         ( "Coefficient of Variation" >:: fun _ ->
           assert_equal
             (Probstats.coefficient_of_variation [ 1.0; 2.0; 3.0; 4.0; 5.0 ])
             (sqrt 2.0 /. 3.0 *. 100.) );
       ]

let _ = run_test_tt_main tests
