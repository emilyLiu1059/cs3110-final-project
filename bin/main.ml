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
