open Core.Std

module Id : sig
  type t with sexp
  val create : unit -> t
  include Comparable.S with type t := t
end = struct
  include Int
  let count = ref Int.min_value
  let create () = (incr count; !count)
end

(* We represent a conjunction of difference logic constraints by a
   directed graph where vertices correspond to natural number valued
   variables and an edge from [a] to [b] with weight [k] means that
   [a + k <= b] or, equivalently, [k <= b - a].  Therefore, larger
   [k] values mean stronger constraints.

   This representation has some nice properties.  First, the weight
   of a path means the same thing as the weight of a single edge.

               k1      k2                    k1 <= b - a
            a ----> b ----> c            + ( k2 <= c - b )
                                        ==================
                k1 + k2                 k1 + k2 <= c - a
             a ---------> c

  A cycle in the graph with weight [k] means that k <= a - a = 0.
  If [k] is positive, then this means that the constraints along the
  cycle are contradictory. *)

type vertex = var

and var = {
  id : Id.t;
  mutable lower_bound : int; (* Always >= 0 *)
  mutable outgoing : edge Id.Map.t;
}

and edge = {
  mutable weight : int;
  target : vertex;
} with sexp

module T = struct
  module T1 = struct
    type t = var with sexp
    let equal t1 t2 = Id.equal t1.id t2.id
    let compare t1 t2 = Id.compare t1.id t2.id
  end
  include T1
  include Comparable.Make (T1)
end

let lower_bound t = t.lower_bound

let var () = {
  id = Id.create ();
  lower_bound = 0;
  outgoing = Id.Map.empty;
}

exception Inconsistent

(* invariant: v >= v' + k *)
let rec test_edge ?(visited = Id.Set.empty) v v' k' =
  if v.lower_bound < v'.lower_bound + k' then begin
    v.lower_bound <- v'.lower_bound + k';
    let visited = Id.Set.add visited v'.id in
    propagate visited v;
  end

and propagate visited v =
  if Id.Set.mem visited v.id then raise Inconsistent; (*[B]*)
  Id.Map.iter v.outgoing ~f:(fun ~key:_ ~data:e ->
    test_edge ~visited e.target v e.weight)

(* explanations for the two [Inconsistent] exceptions raised above:

  [A]

    The pseudo-variable [zero]'s lower bound should never be
    increased since it is really a constant.

  [B]

    At this point, we've traversed a cycle by increasing the
    lower_bound field of each vertex along the way.  This is only
    possible if there is a cycle in the graph corresponding to the
    following inqualities:

       v1 - v2 >= k1
       v2 - v3 >= k2
           ...
       vn - v1 >= kn

    In this cycle, [v1.lower_bound] has been updated twice, and its
    current lower_bound is now

        v1.lower_bound
      = v2.lower_bound + k1
      = v3.lower_bound + k2 + k1
      = ...
      = v1.lower_bound + kn + ... + k1

    where the final v1.lower_bound is the original value at v1.
    Since v1.lower_bound is updated only with increments, this means
    that

      kn + ... + k1 > 0

    However the inequalities in the cycle sum to yield

      0 = v1 - v1 >= k1 + ... + kn

    Therefore we've hit a contradiction.
*)

let set_geq v (`Plus (v', k')) = (* v >= v' + k' *)
  begin match Id.Map.find v'.outgoing v.id with
  | Some e ->
      if k' > e.weight then begin
        e.weight <- k';
        test_edge v v' k';
      end
  | None ->
      let e = { weight = k'; target = v } in
      v'.outgoing <- Id.Map.add v'.outgoing ~key:v.id ~data:e;
      test_edge v v' k';
  end

let set_geq a e =
  try set_geq a e; `Ok
  with Inconsistent -> `Inconsistent

let set_leq a (`Plus (b, k)) = set_geq b (`Plus (a, -k))
let set_lt a (`Plus (b, k)) = set_leq a (`Plus (b, k - 1))
let set_gt a (`Plus (b, k)) = set_geq a (`Plus (b, k + 1))

type path_weight = Negative_infinity | Weight of int

(* strongest consequence of current constraints on the variables
   given. *)
let limit vars =
  let vertices =
    let rec gather visited = function
      | [] -> visited
      | var :: vars ->
          if Set.mem visited var then
            gather visited vars
          else
            gather (Set.add visited var) vars
    in
    gather T.Set.empty (Set.to_list vars)
  in
  (* Floyd-Warshall algorithm for all-pairs longest-paths. *)
  let c =
    T.Set.fold vertices ~init:T.Map.empty ~f:(fun c u ->
      T.Map.add c ~key:u ~data:begin
        Id.Map.fold u.outgoing ~init:T.Map.empty
          ~f:(fun ~key:_ ~data:e c_u ->
            T.Map.add c_u ~key:e.target ~data:(Weight e.weight))
      end)
  in
  let get c u v =
    match T.Map.find c u with
    | None -> Negative_infinity
    | Some c_u ->
        match T.Map.find c_u v with
        | None -> Negative_infinity
        | Some k -> k
  in
  let set c u v k =
    let c_u =
      match T.Map.find c u with
      | None -> T.Map.empty
      | Some map -> map
    in
    T.Map.add c ~key:u ~data:(T.Map.add c_u ~key:v ~data:k)
  in
  let plus a b =
    match (a, b) with
    | (Weight a, Weight b) -> Weight (a + b)
    | (Negative_infinity, _)
    | (_, Negative_infinity) -> Negative_infinity
  in
  let greater_than a b =
    match (a, b) with
    | (Weight a, Weight b) -> a > b
    | (Weight _, Negative_infinity) -> true
    | (Negative_infinity, _) -> false
  in
  let c =
    Set.fold vertices ~init:c ~f:(fun c w ->
      Set.fold vertices ~init:c ~f:(fun c u ->
        Set.fold vertices ~init:c ~f:(fun c v ->
          let c_uv = get c u v in
          let c_uw = get c u w in
          let c_wv = get c w v in
          if greater_than (plus c_uw c_wv) c_uv then
            set c u v (plus c_uw c_wv)
          else
            c)))
  in
  Map.fold c ~init:[] ~f:(fun ~key:u ~data:c_u acc ->
    if Set.mem vars u then
      Map.fold c_u ~init:acc ~f:(fun ~key:v ~data:w acc ->
        if Set.mem vars v then
          match w with
          | Negative_infinity -> acc
          | Weight k -> `Geq (v, `Plus (u, k)) :: acc
        else
          acc)
    else
      acc)

include T

type s = [`Plus of t * int]

TEST_MODULE = struct

  open Int.Replace_polymorphic_compare

  let ge a b = set_geq a b
  let le a b = set_leq a b
  let lt a b = set_lt a b
  let gt a b = set_gt a b

  let n = ref 0

  let die () =
    print_endline ("ERROR! \
                    open the file in vi and run the following two line command\n\
                    /ANC" ^ "HOR\n" ^ Int.to_string !n ^ "/[ynk]ay");
    assert false;
  ;;

  let yay r a b = (incr n; match r a b with `Ok -> () | `Inconsistent -> die ())
  let nay r a b = (incr n; match r a b with `Ok -> die () | `Inconsistent -> ())
  let kay   a k = (incr n; if k <> lower_bound a then die ())

  (* ANCHOR *)

  TEST_UNIT =
    let x = var () in
    kay x 0;
    yay le x (`Plus (x, 0));
    kay x 0;
  ;;

  TEST_UNIT =
    let x = var () in
    yay ge x (`Plus (x, 0));
    kay x 0;
  ;;

  TEST_UNIT =
    let x = var () in
    nay lt x (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    nay gt x (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    yay gt x (`Plus (y, 0));
    kay x 1;
    kay y 0;
    nay ge y (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    yay lt x (`Plus (y, 0));
    kay x 0;
    kay y 1;
    nay le y (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    yay le x (`Plus (y, 0));
    kay x 0;
    kay y 0;
    nay lt y (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    yay ge x (`Plus (y, 0));
    kay x 0;
    kay y 0;
    nay gt y (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 0;
    yay ge y (`Plus (z, 1));
    kay x 2;
    kay y 1;
    kay z 0;
    nay ge z (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 0;
    yay ge y (`Plus (z, 1));
    kay x 2;
    kay y 1;
    kay z 0;
    yay ge z (`Plus (x, -2));
    kay x 2;
    kay y 1;
    kay z 0;
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 0;
    yay ge y (`Plus (z, 1));
    kay x 2;
    kay y 1;
    kay z 0;
    nay ge z (`Plus (x, -1));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 0;
    yay ge y (`Plus (z, 1));
    kay x 2;
    kay y 1;
    kay z 0;
    nay ge z (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 0;
    yay ge z (`Plus (x, 0));
    kay x 1;
    kay y 0;
    kay z 1;
    nay ge y (`Plus (z, 1));
  ;;

  TEST_UNIT =
    let x = var () in
    let y = var () in
    let z = var () in
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge z (`Plus (x, 0));
    kay x 0;
    kay y 0;
    kay z 0;
    yay ge x (`Plus (y, 1));
    kay x 1;
    kay y 0;
    kay z 1;
    nay ge y (`Plus (z, 1));
  ;;

  TEST_UNIT =
    let w = var () in
    let x = var () in
    let y = var () in
    let z = var () in
    kay w 0;
    kay x 0;
    kay y 0;
    kay z 0;
    yay lt w (`Plus (x, 0));
    kay w 0;
    kay x 1;
    kay y 0;
    kay z 0;
    yay le y (`Plus (z, 0));
    kay w 0;
    kay x 1;
    kay y 0;
    kay z 0;
    yay lt x (`Plus (y, 0));
    kay w 0;
    kay x 1;
    kay y 2;
    kay z 2;
    nay le z (`Plus (x, 0));
  ;;

  TEST_UNIT =
    let w = var () in
    let x = var () in
    let y = var () in
    let z = var () in
    yay lt w (`Plus (x, 0));
    kay w 0;
    kay x 1;
    kay y 0;
    kay z 0;
    yay le y (`Plus (z, 0));
    kay w 0;
    kay x 1;
    kay y 0;
    kay z 0;
    yay lt x (`Plus (y, 0));
    kay w 0;
    kay x 1;
    kay y 2;
    kay z 2;
    yay le z (`Plus (x, 2));
    kay w 0;
    kay x 1;
    kay y 2;
    kay z 2;
  ;;

  TEST_UNIT =
    let x1 = var () in
    let x2 = var () in
    let x3 = var () in
    let x4 = var () in
    let x5 = var () in
    let x6 = var () in
    let x7 = var () in
    let x8 = var () in
    let x9 = var () in
    let x10 = var () in
    let x11 = var () in
    let x12 = var () in
    let x13 = var () in
    let x14 = var () in
    let x15 = var () in
    let x16 = var () in
    let x17 = var () in
    let x18 = var () in
    let x19 = var () in
    let x20 = var () in
    let x21 = var () in
    let x22 = var () in
    let x23 = var () in
    let x24 = var () in
    let x25 = var () in

    let axiom s1 s2 = yay lt s1 (`Plus (s2, 0)) in
    let rule s1 s2 s3 = yay le s1 (`Plus (s3, 0)); yay le s2 (`Plus (s3, 0)) in

    axiom x1 x2;
    axiom x4 x5;
    axiom x6 x7;
    axiom x11 x15;
    axiom x12 x17;
    axiom x13 x20;
    axiom x14 x22;
    axiom x10 x25;

    rule x2 x1 x3;
    rule x5 x7 x8;
    rule x4 x3 x6;
    rule x10 x15 x16;
    rule x16 x17 x18;
    rule x18 x10 x19;
    rule x10 x20 x21;
    rule x21 x22 x23;
    rule x19 x23 x24;
    rule x25 x24 x9;
  ;;

end
