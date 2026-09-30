(** [count n] is [n], computed by adding 1 to itself [n] times.  That is,
    this function counts up from 1 to [n]. *)
let rec count n =
  if n = 0 then 0 else 1 + count (n - 1)

let rec count_aux n acc =
  match n with
  | 0 -> 0
  | _ ->
    let acc = 0 in acc
    (* count_aux *)

