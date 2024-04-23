val mean : float list -> float
(** [mean lst] calculates the mean (average) of a list of floating point
    numbers. *)

val median : float list -> float
(** [median lst] calculates the median of a list of floating point numbers. *)

val variance : float list -> float
(** [variance lst] calculates the variance of a list of floating point numbers. *)

val sd : float list -> float
(** [sd lst] calculates the standard deviation of a list of floating point
    numbers. *)

val quantile : float -> float list -> float
(** [quantile q lst] calculates the q-th quantile of a list of floating point
    numbers, where q is a float between 0. and 1. (e.g., 0.25 for the first
    quartile). *)

val range : float list -> float
(** [range lst] calculates the range of a list of floating point numbers, which
    is the difference between the maximum and minimum values. *)

val mode : float list -> float
(** [mode lst] calculates the mode of a list of floating point numbers. In case
    of multiple modes, it returns one of them at random. *)

val permutation : int -> int -> int
(** [permutation n r] calculates the number of ways to choose r elements from a
    set of n elements without repetition and with order. *)

val combination : int -> int -> int
(** [combination n r] calculates the number of ways to choose r elements from a
    set of n elements without repetition and without regard to order. *)

val skewness : float list -> float
(** [skewness lst] calculates the skewness of a list of floating point numbers.
    Skewness measures the asymmetry of the distribution of values in the list.
    Positive skewness indicates a distribution with the tail extending towards
    more positive values; negative skewness indicates a distribution with the
    tail extending towards more negative values. *)

val z_score : float -> float list -> float
(** [z_score x lst] computes the z-score of the value [x] in the context of the
    list [lst]. The z-score is the number of standard deviations [x] is from the
    mean of [lst]. *)

val iqr : float list -> float
(** [iqr lst] calculates the interquartile range of the list [lst], which is the
    difference between the 75th percentile and the 25th percentile. This
    measures the middle 50% spread of the data, providing a robust measure of
    variability that is less sensitive to outliers. *)

val coefficient_of_variation : float list -> float
(** [coefficient_of_variation lst] calculates the coefficient of variation of
    the list [lst], expressed as a percentage. It represents the ratio of the
    standard deviation to the mean, providing a normalized measure of dispersion
    relative to the mean of the data. *)

val correlation : float list -> float list -> float
(** [correlation lst1 lst2] calculates the Pearson correlation coefficient
    between two datasets, [lst1] and [lst2]. This coefficient measures the
    linear relationship between the datasets, with a value between -1 and 1
    where 1 indicates a perfect positive linear relationship, -1 indicates a
    perfect negative linear relationship, and 0 indicates no linear correlation. *)

val cdf : float -> float list -> float
(** [cdf x lst] calculates the cumulative distribution function (CDF) at the
    value [x] for the list [lst]. The function returns the probability that a
    randomly selected value from [lst] is less than or equal to [x], providing a
    basic way to evaluate the distribution of values. *)

val linear_regression : float list -> float list -> float * float
(** [linear_regression xs ys] calculates the parameters of a simple linear
    regression for the data points represented by [xs] (independent variable)
    and [ys] (dependent variable). Returns a tuple (slope, intercept) where
    'slope' is the rate of change in [ys] per unit change in [xs] and
    'intercept' is the expected value of [ys] when [xs] is zero. *)
