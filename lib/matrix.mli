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

(* [to_string a] converts a to a string *)
val to_string : float array array -> string
