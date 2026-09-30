type side =
  | Buy
  | Sell

type t =
  { id : int
  ; side : side
  ; price : int
  ; qty : int
  }

val add : t -> t list * t
val cancel : t -> int -> bool -> t
