(** *)

open Rss;;
open Ers_types;;

let set_item_source src item =
  match item.Rss.item_source with
    None -> { item with Rss.item_source = Some src }
  | _ -> item

let get_source = function
| Channel ch -> ch
| Url url ->
    let contents = Ers_curl.get url in
    let ch = fst (Ers_io.channel_of_string contents) in
    let src = { Rss.src_url = url ; src_name = ch.Rss.ch_title } in
    { ch with Rss.ch_items = List.map (set_item_source src) ch.Rss.ch_items }


let get_source_channels query =
  List.map get_source query.q_sources
;;

module UMap = Map.Make
  (struct type t = Neturl.url let compare = Ers_types.compare_url end)

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
  let (nolink, map) = List.fold_left f_chan ([], UMap.empty)
    ((match target with None -> [] | Some ch -> [ch]) @ channels)
  in
  let items = UMap.fold (fun _ item acc -> item :: acc) map nolink in
  let items = Rss.sort_items_by_date items in
  match target, channels with
    Some ch, _
  | None, ch :: _ -> { ch with ch_items = items }
  | None, [] -> failwith "No channel to merge"
;;
  