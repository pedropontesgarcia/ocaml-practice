type t

type side =
  | Buy
  | Sell

type req =
  { id : int
  ; side : side
  ; price : int
  ; qty : int
  }

val add : t -> req -> req list
val cancel : t -> int -> unit
