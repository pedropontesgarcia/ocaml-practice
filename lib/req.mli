type t

type side =
  | Buy
  | Sell

type req = int * side * int * int

val add : req -> req list
val cancel : int -> bool
