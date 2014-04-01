(******************************************************************************)
(*               Erssical                                                     *)
(*                                                                            *)
(*   Copyright (C) 2013 Institut National de Recherche en Informatique        *)
(*   et en Automatique. All rights reserved.                                  *)
(*                                                                            *)
(*   This program is free software; you can redistribute it and/or modify     *)
(*   it under the terms of the GNU Lesser General Public License version      *)
(*   3 as published by the Free Software Foundation.                          *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU Library General Public License for more details.                     *)
(*                                                                            *)
(*   You should have received a copy of the GNU Library General Public        *)
(*   License along with this program; if not, write to the Free Software      *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                 *)
(*   02111-1307  USA                                                          *)
(*                                                                            *)
(*   Contact: Maxence.Guesdon@inria.fr                                        *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

(** *)

open Rss;;
open Ers_types;;

(*c==v=[List.list_diff]=1.0====*)
let list_diff ?(pred=(=)) l1 l2 =
  List.fold_right
    (fun el acc ->
       if not (List.exists (pred el) l2) then
         el :: acc
       else
         acc
    )
    l1 []
(*/c==v=[List.list_diff]=1.0====*)

let merge_string_list l1 l2 = l1 @ (list_diff l2 l1);;

let merge_event_info item ev =
  match item.item_data with
    None -> { item with item_data = Some ev }
  | Some evi ->
      let ev_link =
        match evi.ev_link, ev.ev_link with
          Some u, _ -> Some u
        | None, x -> x
      in
      let ev_level =
        match evi.ev_level, ev.ev_level with
          Some l, _ -> Some l
        | None, x -> x
      in
      let ev_type =
        match evi.ev_type, ev.ev_type with
          Some t, _ -> Some t
        | None, x -> x
      in
      let ev_keywords = merge_string_list evi.ev_keywords ev.ev_keywords in
      let ev_speakers = merge_string_list evi.ev_speakers ev.ev_speakers in
      let ev_organizers = merge_string_list evi.ev_organizers ev.ev_organizers in
      let ev_location =
        match evi.ev_location, ev.ev_location with
          Some l, _ -> Some l
        | None, x -> x
      in
      let ev_audience =
        match evi.ev_audience, ev.ev_audience with
          Some a, _ -> Some a
        | None, x -> x
      in
      (* specify all fields to get a compilation error when we add fields in the future *)
      let ev =
        { ev_link ; ev_level ; ev_type ; ev_keywords ; ev_speakers ; ev_organizers ;
          ev_location ; ev_audience ; ev_start = evi.ev_start ; ev_end = evi.ev_end ;
        }
      in
      { item with item_data = Some ev }
;;

let set_item_source src item =
  match item.Rss.item_source with
    None -> { item with Rss.item_source = Some src }
  | _ -> item

let get_source ?cache ?(add_event_info=true) = function
| Channel ch -> (Some ch, [])
| Url (url, ev) ->
    try
      let contents =
        match cache with
          None -> Ers_curl.get url
        | Some t -> Ers_cache.get t url
      in
      let (ch, errors) =
        try Ers_io.channel_of_string contents
        with Failure msg -> failwith ((Ers_types.string_of_url url)^": "^msg)
      in
      let errors = List.map
        (fun msg -> (Ers_types.string_of_url url)^": "^msg)
          errors
      in
      let src = { Rss.src_url = url ; src_name = ch.Rss.ch_title } in
      let f_item item =
        let item = set_item_source src item in
        if add_event_info then
          merge_event_info item ev
        else
          item
      in
      let items = List.map f_item ch.Rss.ch_items in
      (Some { ch with Rss.ch_items = items }, errors)
    with
    e ->
        let msg = match e with
            Failure msg -> msg
          | _ -> Printexc.to_string e
        in
        (None, [msg])

let get_source_channels ?cache query =
  List.map (get_source ?cache) query.q_sources
;;

let get_target_channel ?cache query =
  match query.q_target with
    None -> None
  | Some source -> 
      let (ch, errors) = get_source ?cache ~add_event_info: false source in
      match ch with
        None -> None
      | Some ch -> Some (ch, errors)
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

let execute ?cache ?rtype query =
  let ret_typ = match rtype with None -> query.q_type | Some t -> t in
  (match ret_typ, query.q_tmpl with
     Xtmpl, None -> failwith "Missing template in query"
   | _ -> ()
  );
  let target_errors = ref [] in
  let source_errors = ref [] in
  try
    let channels = get_source_channels ?cache query in
    let (channels, errors) = List.fold_left
      (fun (acc_ch, acc_errors) (ch, errors) ->
        match ch with
          None -> (acc_ch, (List.rev errors) @ acc_errors)
        | Some ch -> (ch :: acc_ch, (List.rev errors) @ acc_errors)
      )
        ([], []) channels
    in
    let channels = List.rev channels in
    source_errors := List.rev errors ;
    let target = get_target_channel ?cache query in
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
    | Xtmpl ->
        match query.q_tmpl with
          None -> assert false
        | Some tmpl -> Res_xtmpl (Ers_xtmpl.apply_template tmpl channel)
  with
    e when ret_typ = Debug ->
      begin
        match e with
          Sys_error s | Failure s ->
            Res_debug (String.concat "\n" (("Error: "^s) :: !target_errors @ !source_errors))
        | _ -> Res_debug (Printexc.to_string e)
      end
;;
