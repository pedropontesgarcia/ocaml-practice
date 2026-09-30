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

val add : t -> t * req list
val cancel : t -> int -> t * bool
