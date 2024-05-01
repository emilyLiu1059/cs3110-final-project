(* Mathematical Constants *)
val pi : float (* Mathematical constant Pi *)
val e : float (* Mathematical constant e *)

(* Conversion Functions *)
val rad_to_deg : float -> float
(** Converts radians to degrees. *)

val deg_to_rad : float -> float
(** Converts degrees to radians. *)

(* Polynomial Operations *)
val eval : float -> float list -> float
(** Evaluates a polynomial at a given x-value. The polynomial is represented as
    a list of coefficients. *)

val deri_once : float list -> float list
(** Computes the derivative of a polynomial represented by a list of
    coefficients. Returns the coefficients of the resulting polynomial. *)

val deri : int -> float list -> float list
(** Computes the n-th derivative of a polynomial. *)

val inte_once : float -> float list -> float list
(** Computes a single indefinite integral (antiderivative) of a polynomial and
    adds a constant C. *)

val add : float list -> float list -> float list
(** Adds two polynomials represented by their coefficient lists. *)

val subtract : float list -> float list -> float list
(** Subtracts the second polynomial from the first, both represented by their
    coefficient lists. *)

val multiply : float list -> float list -> float list
(** Multiplies two polynomials represented by their coefficient lists. *)

val divide_polynomials : float list -> float list -> float list * float list
(** Divides one polynomial by another and returns a tuple of quotient and
    remainder polynomial coefficients. *)

val gcd_polynomial : float list -> float list -> float list
(** Calculates the greatest common divisor of two polynomials. *)

val compose_polynomials : float list -> float list -> float list
(** Composes two polynomials, returning the coefficients of the resulting
    polynomial. *)

val eval_at_points : float list -> float list -> float list
(** Evaluates a polynomial at multiple points provided in a list, returning the
    results in a list. *)

(* Utility Functions *)
val degree : float list -> int
(** Returns the degree of a polynomial represented by a list of coefficients. *)

val string_of_float_list : float list -> string
(** Converts a list of float numbers into a comma-separated string. Useful for
    debugging and logging. *)

val poly_to_string : float list -> string
(** Converts a list of polynomial coefficients into a human-readable string
    representation of the polynomial. *)
