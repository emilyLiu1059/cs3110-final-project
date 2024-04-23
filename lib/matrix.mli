(* [identity_matrix n] creates a n x n identity matrix *)
val identity_matrix : int -> float array array

(* [add a b] adds matrix a and matrix b together. Requires: a and b have the
   same dimensions *)
val add : float array array -> float array array -> float array array

(* [subtract a b] subtracts matrix b from matrix a. Requires: a and b have the
   same dimensions *)
val subtract : float array array -> float array array -> float array array

(* [transpose a] returns the transpose of a *)
val transpose : float array array -> float array array

(* [multiply a b] performs matrix multiplication on a and b. Requires: number of
   columns of a = number of rows of b *)
val multiply : float array array -> float array array -> float array array

(* [scalar_add matrix scalar] performs addition between a scalar and a matrix *)
val scalar_add : float array array -> float -> float array array

(* [scalar_subtract matrix scalar] performs addition between a scalar and a
   matrix *)
val scalar_subtract : float array array -> float -> float array array

(* [scalar_multiply matrix scalar] performs multiplication between a scalar and
   a matrix *)
val scalar_multiply : float array array -> float -> float array array

(* [scalar_divide matrix scalar] performs division between a scalar and a
   matrix *)
val scalar_divide : float array array -> float -> float array array

(* [determinant matrix] calculates the determinant of a matrix. Requires: matrix
   is square (has the same number of cols and rows) *)
val determinant : float array array -> float

(* [swap_rows matrix row1 row2] swaps rows row1 and row2 of a matrix *)
val swap_rows : float array array -> int -> int -> float array array

(* [swap_cols matrix col1 col2] swaps columns col1 and col2 of a matrix *)
val swap_cols : float array array -> int -> int -> float array array

(* [row_reduce matrix] performs row reduction on matrix *)
val row_reduce : float array array -> float array array

(* [rank matrix] finds the rank of a matrix *)
val rank : float array array -> int

(* [to_string a] converts a to a string *)
val to_string : float array array -> string
