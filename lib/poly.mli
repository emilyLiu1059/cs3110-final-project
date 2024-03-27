(* Signature for the module containing mathematical utilities and operations on
   polynomials. *)

val pi : float
(* The mathematical constant pi *)

val e : float
(* The base of the natural logarithm *)

val rad_to_deg : float -> float
(* Convert radians to degrees *)

val deg_to_rad : float -> float
(* Convert degrees to radians *)

val eval : float -> float list -> float
(* Evaluate a polynomial at a given value of x *)

val deri_once : float list -> float list
(* Derive a polynomial once *)

val deri : int -> float list -> float list
(* Derive a polynomial n times *)

val inte_once : float -> float list -> float list
(* Integrate a polynomial once, adding a constant c *)

val add : float list -> float list -> float list
(* Add two polynomials *)

val degree : float list -> int
(* Calculate the degree of a polynomial *)

val multiply : float list -> float list -> float list
(* Multiply two polynomials *)

val string_of_float_list : float list -> string
(* Convert the float list to string*)
