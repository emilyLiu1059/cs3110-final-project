val mean : float list -> float
(** [mean lst] calculates the mean (average) of a list of floating point numbers. *)

val median : float list -> float
(** [median lst] calculates the median of a list of floating point numbers. *)

val variance : float list -> float
(** [variance lst] calculates the variance of a list of floating point numbers. *)

val sd : float list -> float
(** [sd lst] calculates the standard deviation of a list of floating point numbers. *)

val quantile : float -> float list -> float
(** [quantile q lst] calculates the q-th quantile of a list of floating point numbers,
    where q is a float between 0. and 1. (e.g., 0.25 for the first quartile). *)

val range : float list -> float
(** [range lst] calculates the range of a list of floating point numbers,
    which is the difference between the maximum and minimum values. *)

val mode : float list -> float
(** [mode lst] calculates the mode of a list of floating point numbers.
    In case of multiple modes, it returns one of them at random. *)

val permutation : int -> int -> int
(** [permutation n r] calculates the number of ways to choose r elements
    from a set of n elements without repetition and with order. *)

val combination : int -> int -> int
(** [combination n r] calculates the number of ways to choose r elements
    from a set of n elements without repetition and without regard to order. *)
