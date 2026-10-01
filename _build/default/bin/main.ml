(** [count n] is [n], computed by adding 1 to itself [n] times.  That is,
    this function counts up from 1 to [n]. *)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let count n =
  let rec count_aux n acc =
    match n with
    | 0 -> acc
    | _ -> count_aux (n - 1) (acc + 1)
  in
  count_aux n 0
;;

(** [fact n] is [n] factorial. *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let fact n =
  let rec fact_aux n acc =
    match n with
    | 0 -> acc
    | _ -> fact_aux (n - 1) (acc * n)
  in
  fact_aux n 0
;;
