val add : float -> float -> float
(** [add a b] adds floats [a] and [b] together. *)

val subtract : float -> float -> float
(** [subtract a b] subtract float [a] by float [b]. *)

val multiply : float -> float -> float
(** [multiply a b] multiplies floats [a] and [b] together. *)

val divide : float -> float -> float
(** [divide a b] divides float [a] by float [b]. Requires that [b] is not 0. *)

val pow : float -> int -> float
(** [pow a b] takes [a] to the power of [b]. Requires that [b] is greater than
    or equal to 0. *)

val nth_root : float -> int -> float
(** [nth_root a b] takes the [b]th root of [a]. Requires that [b] is greater
    than or equal to 1. *)

val abs : float -> float
(** [abs a] takes the absolute value of [a]. *)

val gcd : int -> int -> int
(** [gcd a b] takes the greatest common denominator of [a] and [b]. *)
