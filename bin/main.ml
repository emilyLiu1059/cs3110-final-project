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
  print_endline (Poly.pi |> Poly.deg_to_rad |> string_of_float)

let () =
  print_string "Evaluate polynomial [2, 3, 4, 2] at a given value of 2.0.";
  print_endline (Poly.eval 2. [ 2.; 3.; 4.; 2. ] |> string_of_float)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] once.";
  print_endline (Poly.deri_once [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

let () =
  print_string "Derive the polynomial [2, 3, 4, 2] 2 times.";
  print_endline (Poly.deri 2 [ 2.; 3.; 4.; 2. ] |> Poly.string_of_float_list)

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
