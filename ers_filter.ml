(** *)

open Ers_types;;
module S = Ers_types.ItemSet;;

module SMap = Ers_types.SMap

let pred_string re =
  let pred s = Str.string_match re s 0 in
  List.exists pred
;;

let pred_tech re = let pred = pred_string re in fun ev -> pred ev.ev_tech;;
let pred_scidom re = let pred = pred_string re in fun ev -> pred ev.ev_scidom;;
let pred_speakers re = let pred = pred_string re in fun ev -> pred ev.ev_speakers;;
let pred_organizers re = let pred = pred_string re in fun ev -> pred ev.ev_organizers;;

let preds_contains =
  [
    Ers_io.tag_tech_short, pred_tech ;
    Ers_io.tag_scidom_short, pred_scidom ;
    Ers_io.tag_speakers_short, pred_speakers ;
    Ers_io.tag_organizers_short, pred_organizers ;
  ]
;;

let compile_contain_pred field re =
  try
    let pred =  (List.assoc field preds_contains) re in
    fun item ->
      match item.Rss.item_data with
        None -> false
      | Some ev -> pred ev
  with Not_found ->
    failwith ("No predicate for field \"" ^ field ^ "\"")
;;

let compile_contain_filter field re =
  let pred = compile_contain_pred field re in
  fun set -> S.filter pred set

let is_before d1 d2 =
  match d1, d2 with
    None, None -> true
  | Some _, None ->
      (* a date and no "before constraint" => ok *)
      true
  | None, Some _ ->
      (* no date and a "before constraint" => ok *)
      true
  | Some d, Some const ->
      Pervasives.compare (Netdate.since_epoch d) (Netdate.since_epoch const) < 0
;;

let is_after d1 d2 =
  match d1, d2 with
    None, None -> true
  | Some _, None ->
      (* a date and no "after constraint" => ok *)
      true
  | None, Some _ ->
      (* no date and a "after constraint" => ok *)
      true
  | Some d, Some const ->
      Pervasives.compare (Netdate.since_epoch d) (Netdate.since_epoch const) >= 0
;;

let compile_date_pred f after before =
  fun item ->
    let date = f item in
    is_after date after && is_before date before
;;


let compile_date_filter f after before =
  match after, before with
    None, None ->
      (fun set -> set)
  | _ ->
      let pred = compile_date_pred f after before in
      (fun set -> S.filter pred set)
;;

let compile_start_date_filter =
  let f item =
    match item.Rss.item_data with
      None -> item.Rss.item_pubdate
    | Some ev -> ev.ev_start
  in
  compile_date_filter f
;;


let compile_end_date_filter =
  let f item =
    match item.Rss.item_data with
      None -> None
    | Some ev -> ev.ev_end
  in
  compile_date_filter f
;;

let rec compile_filter = function
  Not f ->
    let f = compile_filter f in
    (fun set -> S.diff set (f set))
| Or l ->
    let l = List.map compile_filter l in
    let f_or base_set set f = S.union set (f base_set) in
    (fun set -> List.fold_left (f_or set) S.empty l)
| And l ->
    let l = List.map compile_filter l in
    let f_and set f = S.inter set (f set) in
    (fun set -> List.fold_left f_and set l)
| StartDate (after, before) -> compile_start_date_filter after before
| EndDate (after, before) -> compile_end_date_filter after before
| Contains (field, connector, values) ->
    let filters = List.map (compile_contain_filter field) values in
    match connector with
      Conn_or ->
        let f_or base_set set f = S.union set (f base_set) in
        (fun set -> List.fold_left (f_or set) S.empty filters)
    | Conn_and ->
        let f_and set f = S.inter set (f set) in
        (fun set -> List.fold_left f_and set filters)

let filter f ch =
  let filter = compile_filter f in
  let set = List.fold_right
    Ers_types.ItemSet.add ch.Rss.ch_items Ers_types.ItemSet.empty
  in
  let set = filter set in
  let items = Rss.sort_items_by_date (Ers_types.ItemSet.elements set) in
  { ch with Rss.ch_items = items }
;;