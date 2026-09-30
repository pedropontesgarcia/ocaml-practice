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
  { mutable sells : ReqPqueue.t
  ; mutable buys : ReqPqueue.t
  ; del_cache : (int, unit) Hashtbl.t
  }

let rec buy_n t n p =
  let min_elem = ReqPqueue.get_min_elt t.sells in
  if min_elem.price <= p
  then (
    let n_remove = Int.min min_elem.qty n in
    ())
;;

let add t req =
  match req.side with
  | Sell -> t.buys
  | Buy -> ()
;;

let cancel t id =
  Hashtbl.replace t.del_cache id ();
  if Hashtbl.length t.del_cache > 100
  then (
    let filtered rid acc req =
      if req.id <> rid then ReqPqueue.add acc req;
      acc
    in
    let remove rid pq =
      ReqPqueue.fold_unordered (filtered rid) (ReqPqueue.create ()) pq
    in
    let remove_all t rid =
      t.sells <- remove rid t.sells;
      t.buys <- remove rid t.buys
    in
    let remove_all_wrapper t rid () = remove_all t rid in
    Hashtbl.iter (remove_all_wrapper t) t.del_cache;
    Hashtbl.reset t.del_cache)
;;
