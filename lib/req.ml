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
  { iid : int
  ; iside : side
  ; iprice : int
  ; mutable iqty : int
  ; itime : int
  }

type hit =
  { inc_id : int
  ; rest_id : int
  ; hit_price : int
  ; hit_qty : int
  }

module Req = struct
  type t = internal_req

  let compare req1 req2 =
    if req1.iside <> req2.iside then failwith "Incompatible comparison";
    match
      match req1.iside with
      | Sell -> Int.compare req1.iprice req2.iprice
      | Buy -> Int.compare req2.iprice req1.iprice
    with
    | 0 -> Int.compare req1.itime req2.itime
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
    if Hashtbl.mem t.del_cache req.iid
    then (
      Hashtbl.remove t.del_cache req.iid;
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
      if not (Hashtbl.mem t.del_cache req.iid) then ReqPqueue.add acc req;
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
    match req.iside with
    | Buy -> t.sells, t.buys
    | Sell -> t.buys, t.sells
  in
  match peek t pq with
  | None ->
    ReqPqueue.add other req;
    []
  | Some preq
    when if req.iside = Buy then preq.iprice > req.iprice else preq.iprice < req.iprice ->
    ReqPqueue.add other req;
    []
  | Some preq ->
    (match preq.iqty - req.iqty with
     | n when n > 0 ->
       preq.iqty <- n;
       [ { inc_id = req.iid
         ; rest_id = preq.iid
         ; hit_price = preq.iprice
         ; hit_qty = req.iqty
         }
       ]
     | n when n = 0 ->
       pop t pq |> ignore;
       [ { inc_id = req.iid
         ; rest_id = preq.iid
         ; hit_price = preq.iprice
         ; hit_qty = preq.iqty
         }
       ]
     | n ->
       pop t pq |> ignore;
       { inc_id = req.iid
       ; rest_id = preq.iid
       ; hit_price = preq.iprice
       ; hit_qty = preq.iqty
       }
       :: add_aux t { req with iqty = -n })
;;

let add t (req : req) =
  if not (req.qty > 0 && req.price > 0) then failwith "Malformed request";
  let internal_req =
    { iid = req.id
    ; iside = req.side
    ; iprice = req.price
    ; iqty = req.qty
    ; itime = !(t.seq)
    }
  in
  if Hashtbl.mem t.del_cache internal_req.iid then purge ~force:true t;
  incr t.seq;
  add_aux t internal_req
;;

let cancel t id =
  Hashtbl.replace t.del_cache id ();
  purge t
;;
