module type Comparable = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] Returns an integer less than zero if the first argument is
      strictly less than the second, zero if the arguments are equal, and an
      integer greater than zero if the first argument is strictly greater than
      the second. *)
end

exception Invalid_interval

module Make (Bound_compare : Comparable) : sig
  module Bound : sig
    type t =
      | Included of Bound_compare.t
      | Excluded of Bound_compare.t
      | Unbounded

    val compare_lower : t -> t -> int
    (** [compare_lower bound_a bound_b] compares two bounds as lower bounds.
        Returns an integer less than zero if the [bound_a] is strictly less than
        [bound_b], zero if the bounds are equal, and an integer greater than
        zero if [bound_a] is strictly greater than [bound_b]. *)

    val compare_upper : t -> t -> int
    (** [compare_upper bound_a bound_b] compares two bounds as upper bounds *)

    val min_lower : t -> t -> t
    (** [min_lower bound_a bound_b] chooses the minimum lower bound between
        [bound_a] and [bound_b] *)

    val max_upper : t -> t -> t
    (** [max_upper bound_a bound_b] chooses the maximum upper bound between
        [bound_a] and [bound_b] *)
  end

  module Interval : sig
    type t = private
      { low : Bound.t
      ; high : Bound.t
      }

    val create : Bound.t -> Bound.t -> t
    (** [create low high] creates an interval from the [low] bound and the
        [high] bound. Raises [Invalid_interval] if low is not less than high. *)

    val compare : t -> t -> int
    (** [compare ivl_a ivl_b] compares two intervals. Intervals are compared
        using their low bounds. If the lower bounds are equal, the high bound
        are compared. Two bounds are equal iff both their bounds are equal. *)

    val overlap_interval : t -> t -> t option
    (** [overlap_interval ivl_a ivl_b] calculates the interval that is the
        overlap between the two intervals. If there is no overlap, [None] is
        returned.*)

    val overlaps : t -> t -> bool
    (** [overlaps ivl_a ivl_b] returns [true] if the two intervals overlap,
        [false] otherwise. *)
  end

  module Query_results : sig
    type 'a t

    val fold : ('acc -> Interval.t * 'a list -> 'acc) -> 'acc -> 'a t -> 'acc
    (** [fold fn acc results] folds over [results] using function [fn], which
        takes the accumulator and each element as parameters and returns the
        updated accumulator. *)

    val to_list : 'a t -> (Interval.t * 'a list) list
    (** [to_list results] transforms [results] to a list *)
  end

  type 'a t

  val empty : 'a t
  (** [empty] is an empty interval map. *)

  val size : 'a t -> int
  (** [size map] returns the number of values stored in the [map]. Multiple
      values may be stored with each interval, so the number of values is not
      necessarily the same as the number of intervals. *)

  val cardinal : 'a t -> int
  (** [cardinal map] is the same as [size map] *)

  val add : Interval.t -> 'a -> 'a t -> 'a t
  (** [add interval value map] adds [value] to [map] associated with [interval].
      Not tail recursive. *)

  val remove_by : Interval.t -> ('a -> bool) -> 'a t -> 'a t
  (** [remove_by interval value_rm_fn map] removes all values associated with
      [interval] for which [value_rm_fn] returns true in [map]. Not tail
      recursive. *)

  val remove_interval : Interval.t -> 'a t -> 'a t
  (** [remove_interval interval map] removes the interval and all associated
      values from [map]. Not tail recursive. *)

  val find_opt : Interval.t -> 'a t -> 'a list option
  (** [find_opt interval map] finds all values associated with [interval] in
      [map], or [None] if [map] does not contain [interval]. *)

  val find : Interval.t -> 'a t -> 'a list
  (** [find interval map] finds all values associated with [interval] in [map],
      or raises [Not_found] if [map] does not contain [interval]. *)

  val mem : Interval.t -> 'a t -> bool
  (** [mem interval map] returns [true] if [map] contains [interval], and
      [false] otherwise. *)

  val query_interval : Interval.t -> 'a t -> 'a Query_results.t
  (** [query_interval interval map] finds all intervals that intersect
      [interval] in [map] and their associated values. Results are provided as a
      generator, which traverses [map] as results are read. *)

  val query_interval_list : Interval.t -> 'a t -> (Interval.t * 'a list) list
  (** [query_interval interval map] finds all intervals that intersect
      [interval] in [map] and their associated values and returns the results as
      a list. *)
end
