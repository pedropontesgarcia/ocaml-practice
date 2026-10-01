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

type internal_req =
  { id : int
  ; side : side
  ; price : int
  ; mutable qty : int
  ; mutable time : int
  }

let req_of_internal_req ({ id; side; price; qty } : internal_req) =
  { id; side; price; qty }
;;

module Req = struct
  type t = internal_req

  let compare req1 req2 =
    if req1.side <> req2.side then failwith "Incompatible comparison";
    match
      match req1.side with
      | Sell -> Int.compare req1.price req2.price
      | Buy -> Int.compare req2.price req1.price
    with
    | 0 -> Int.compare req1.time req2.time
    | i -> i
  ;;
end

module ReqPqueue = Pqueue.MakeMin (Req)

type t =
  { mutable sells : ReqPqueue.t
  ; mutable buys : ReqPqueue.t
  ; del_cache : (int, unit) Hashtbl.t
  ; seq : int ref
  }

let rec clean t pq =
  match ReqPqueue.min_elt pq with
  | None -> ()
  | Some req ->
    if Hashtbl.mem t.del_cache req.id
    then (
      Hashtbl.remove t.del_cache req.id;
      ReqPqueue.remove_min pq;
      clean t pq)
;;

let pop t pq =
  clean t pq;
  ReqPqueue.pop_min pq
;;

let peek t pq =
  clean t pq;
  ReqPqueue.min_elt pq
;;

(** Minimum size of purge table that triggers a purge. *)
let purge_threshold_floor = 100

(** Denominator for queue size proportion that triggers a purge. *)
let purge_threshold_proportion_denom = 3

let purge ?(force = false) t =
  let purge_aux () =
    let filtered acc req =
      if not (Hashtbl.mem t.del_cache req.id) then ReqPqueue.add acc req;
      acc
    in
    let purge pq = ReqPqueue.fold_unordered filtered (ReqPqueue.create ()) pq in
    t.sells <- purge t.sells;
    t.buys <- purge t.buys;
    Hashtbl.reset t.del_cache
  in
  if force
  then purge_aux ()
  else (
    let purge_threshold =
      max
        purge_threshold_floor
        (max (ReqPqueue.length t.sells) (ReqPqueue.length t.buys)
         / purge_threshold_proportion_denom)
    in
    if Hashtbl.length t.del_cache > purge_threshold then purge_aux ())
;;

let create () =
  { sells = ReqPqueue.create ()
  ; buys = ReqPqueue.create ()
  ; del_cache = Hashtbl.create 100
  ; seq = ref 0
  }
;;

let rec add_aux t req =
  let pq, other =
    match req.side with
    | Buy -> t.sells, t.buys
    | Sell -> t.buys, t.sells
  in
  match peek t pq with
  | None -> []
  | Some preq
    when if req.side = Buy then preq.price > req.price else preq.price > req.price -> []
  | Some preq ->
    (match preq.qty - req.qty with
     | n when n > 0 ->
       preq.qty <- n;
       ReqPqueue.add other { req with qty = req.qty - n };
       [ req_of_internal_req { req with price = preq.price } ]
     | n when n = 0 ->
       [ (pop t pq
          |> Option.get
          |> req_of_internal_req
          |> fun r -> { r with price = preq.price })
       ]
     | n ->
       (pop t pq
        |> Option.get
        |> req_of_internal_req
        |> fun r -> { r with price = preq.price })
       :: add_aux t { req with qty = -n })
;;

let add t (req : req) =
  if not (req.qty > 0 && req.price > 0) then failwith "Malformed request";
  let internal_req =
    { id = req.id; side = req.side; price = req.price; qty = req.qty; time = !(t.seq) }
  in
  if Hashtbl.mem t.del_cache internal_req.id then purge ~force:true t;
  incr t.seq;
  add_aux t internal_req
;;

let cancel t id =
  Hashtbl.replace t.del_cache id ();
  purge t
;;
