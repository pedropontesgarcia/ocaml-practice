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

type hit =
  { inc_id : int
  ; rest_id : int
  ; price : int
  ; qty : int
  }

val add : t -> req -> hit list
val cancel : t -> int -> unit
