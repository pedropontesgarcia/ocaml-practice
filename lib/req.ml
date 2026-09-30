open Pqueue

type side =
  | Buy
  | Sell

type req =
  { id : int
  ; side : side
  ; price : int
  ; qty : int
  }

module Req = struct
  type t = req

  let compare req1 req2 = Int.compare req1.price req2.price
end

module ReqPqueue = Pqueue.MakeMin (Req)

type t =
  { sells : ReqPqueue.t
  ; buys : ReqPqueue.t
  }

let rec buy_n t n p =
  let min_elem = ReqPqueue.get_min_elt t.sells in
  if min_elem.price <= p then
    let n_remove = Int.min min_elem.qty n in
    
;;

let add t req =
  match req.side with
  | Sell -> t.buys
  | Buy -> ()
;;

let cancel t id = ReqPqueue.
