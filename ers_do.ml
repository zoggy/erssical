(** *)

open Rss;;
open Ers_types;;

let set_item_source src item =
  match item.Rss.item_source with
    None -> { item with Rss.item_source = Some src }
  | _ -> item

let get_source = function
| Channel ch -> (ch, [])
| Url url ->
    let contents = Ers_curl.get url in
    let (ch, errors) = Ers_io.channel_of_string contents in
    let src = { Rss.src_url = url ; src_name = ch.Rss.ch_title } in
    ({ ch with Rss.ch_items = List.map (set_item_source src) ch.Rss.ch_items }, errors)


let get_source_channels query =
  List.map get_source query.q_sources
;;

let get_target_channel query =
  match query.q_target with
    None -> None
  | Some source -> Some (get_source source)
;;

module UMap = Map.Make
  (struct type t = Neturl.url let compare = Ers_types.compare_url end)
module SMap = Ers_types.SMap

let merge_channels ?target channels =
  let f_item (nolink, map) item =
    match item.item_link with
      None -> (item :: nolink, map)
    | Some url ->
        try
          ignore(UMap.find url map);
          (nolink, map)
        with
          Not_found ->
            (nolink, UMap.add url item map)
  in
  let f_chan acc ch = List.fold_left f_item acc ch.ch_items in
  let channels = (match target with None -> [] | Some ch -> [ch]) @ channels in
  let (nolink, map) = List.fold_left f_chan ([], UMap.empty) channels in
  let namespaces =
    let f map (ns,url) =
      try ignore (SMap.find ns map); map
      with Not_found ->
        SMap.add ns url map
    in
    let f_ch map ch = List.fold_left f map ch.ch_namespaces in
    let map = List.fold_left f_ch SMap.empty channels in
    SMap.fold (fun ns url acc -> (ns, url) :: acc) map []
  in
  let items = UMap.fold (fun _ item acc -> item :: acc) map nolink in
  let items = Rss.sort_items_by_date items in
  match target, channels with
    Some ch, _
  | None, ch :: _ -> { ch with ch_items = items ; ch_namespaces = namespaces }
  | None, [] -> failwith "No channel to merge"
;;

let execute ?rtype query =
  let ret_typ = match rtype with None -> query.q_type | Some t -> t in
  let target_errors = ref [] in
  let source_errors = ref [] in
  try
    let channels = get_source_channels query in
    let (channels, errors) = List.fold_left
      (fun (acc_ch, acc_errors) (ch, errors) ->
         (ch :: acc_ch, (List.rev errors) @ acc_errors))
        ([], []) channels
    in
    let channels = List.rev channels in
    source_errors := List.rev errors ;
    let target = get_target_channel query in
    let target =
      match target with
        None -> None
      | Some (target, errors) ->
        target_errors := errors ;
        Some target
    in
    let channel = merge_channels ?target channels in
    let channel =
      match query.q_filter with
        None -> channel
      | Some f -> Ers_filter.filter f channel
    in
    match ret_typ with
    | Debug -> Res_debug (String.concat "\n" ("Ok" :: !target_errors @ !source_errors))
    | Rss -> Res_channel channel
    | Ical -> Res_ical (Ers_ical.ical_of_channel channel)
  with
    e when ret_typ = Debug ->
      begin
        match e with
          Sys_error s | Failure s ->
            Res_debug (String.concat "\n" (("Error: "^s) :: !target_errors @ !source_errors))
        | _ -> Res_debug (Printexc.to_string e)
      end
;;
