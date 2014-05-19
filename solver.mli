(* mutable difference logic solver with with natural number valued
   variables *)

open Core.Std

type t
type s = [`Plus of t * int]

include Comparable.S with type t := t

val var : unit -> t

val set_geq : t -> s -> [ `Ok | `Inconsistent ]
val set_leq : t -> s -> [ `Ok | `Inconsistent ]
val set_gt  : t -> s -> [ `Ok | `Inconsistent ]
val set_lt  : t -> s -> [ `Ok | `Inconsistent ]

val limit : Set.t -> [`Geq of t * s] list

val lower_bound : t -> int (* never negative *)
