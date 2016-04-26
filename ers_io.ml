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

module XR = Xtmpl_rewrite
open Rss
open Ers_types

let tag_ =
  let url = Ers_types.string_of_url Ers_types.base_url in
  fun s -> (url, s)

let tag_link = tag_"link"

let tag_level = tag_"level"

let tag_type_short = "type"
let tag_type = tag_ tag_type_short

let tag_keywords_short = "keywords"
let tag_keywords = tag_ tag_keywords_short

let tag_speakers_short = "speakers"
let tag_speakers = tag_ tag_speakers_short

let tag_organizers_short = "organizers"
let tag_organizers = tag_ tag_organizers_short

let tag_location = tag_"location"

let tag_start_short = "startDate"
let tag_start = tag_ tag_start_short

let tag_end_short = "endDate"
let tag_end = tag_ tag_end_short

let tag_audience = tag_"audience"

(** {2 Reading} *)

(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)
(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

let get_att ?(pref="") name atts =
  try Some (List.assoc (pref,name) atts)
  with
    Not_found -> None
;;

let get_elt ?(pref="") name xmls =
  let f = function
    D _ -> false
  | E ((tag,_),_) -> tag = (pref, name)
  in
  try
    match List.find f xmls with
      D _ -> assert false
    | E ((_,atts), subs) -> Some (atts, subs)
  with Not_found -> None
;;

let read_string_items =
  let f acc = function
    E ((("", "item"), _), [D s]) -> s :: acc
  | _ -> acc
  in
  fun cont xmls ->
    cont (List.rev (List.fold_left f [] xmls))
;;

let read_link ev atts xmls =
  match xmls with
    [] -> ev
  | (D u) :: _ ->
      (
       try { ev with ev_link = Some (Ers_types.url_of_string u) }
       with Failure _ -> ev
      )
  | _ -> ev
;;

let read_level ev atts xmls = ev
let read_type ev atts xmls = ev

let read_keywords ev atts xmls =
  let f l = { ev with ev_keywords = ev.ev_keywords @ l } in
  read_string_items f xmls

let read_speakers ev atts xmls =
  let f l = { ev with ev_speakers = ev.ev_speakers @ l } in
  read_string_items f xmls

let read_organizers ev atts xmls =
  let f l = { ev with ev_organizers = ev.ev_organizers @ l } in
  read_string_items f xmls

let read_location ev atts xmls =
  let url =
    match get_att "href" atts with
      None -> None
    | Some s -> Some (Ers_types.url_of_string s)
  in
  let name_pcdata = List.fold_right
   (fun xml acc ->
       match xml with
         D s -> s :: acc
       | _ -> acc
    )
    xmls []
  in
  let name = String.concat " " name_pcdata in
  { ev with ev_location = Some { loc_href = url ; loc_name = name } }

let read_date_pcdata = function
  [D s] ->
    begin
      try Some (Netdate.parse s)
      with _ -> None
    end
| _ -> None
;;

let read_start ev atts xmls =
  match read_date_pcdata xmls with
    None -> ev
  | d -> { ev with ev_start = d }
;;

let read_end ev atts xmls =
  match read_date_pcdata xmls with
    None -> ev
  | d -> { ev with ev_end = d }
;;

let read_audience ev atts xmls = ev

let funs = [
    tag_link, read_link ;
    tag_level, read_level ;
    tag_type, read_type ;
    tag_keywords, read_keywords ;
    tag_speakers, read_speakers ;
    tag_organizers, read_organizers ;
    tag_location, read_location ;
    tag_start, read_start ;
    tag_end, read_end ;
    tag_audience, read_audience ;
  ]

let read_event_xml ev = function
  E ((tag, atts), xmls) ->
    begin
      try (List.assoc tag funs) ev atts xmls
      with Not_found ->
        ev
    end
| D _ -> ev

let read_item_data xmls =
  let ev = Ers_types.event () in
  let ev = List.fold_left read_event_xml ev xmls in
  Some ev
;;

let mime_type_ical = "text/calendar"
let mime_type_rss = "application/rss+xml"
let mime_type_xml = "application/xml"

let q_return_type_of_atts atts =
  match get_att "type" atts with
  | Some s when s = mime_type_ical -> Ical
  | Some s when s = mime_type_xml -> Xtmpl
  | _ -> Rss
;;

let (opts : (unit, Ers_types.event) Rss.opts) = Rss.make_opts ~read_item_data ();;

let channel_of_file file = Rss.channel_t_of_file opts file
let channel_of_string str = Rss.channel_t_of_string opts str

let read_default_event_info xmls =
  (* add prefix is not present, so that we can re-use the read_item_data function *)
  let add_ns = function
    Rss.E ((("",s),atts),subs) ->
      Rss.E ((tag_ s, atts), subs)
  | xml -> xml
  in
  let xmls = List.map add_ns xmls in
  match read_item_data xmls with
    None -> assert false
  | Some ev -> ev
;;

let read_source ?(tag="source") xmls =
  match get_elt tag xmls with
    None -> None
  | Some (atts, subs) ->
      match get_att "href" atts with
        Some s_url ->
          let url = Ers_types.url_of_string s_url in
          let def_event = read_default_event_info subs in
          Some (Url (url, def_event))
      | None ->
          let ch = fst (Rss.channel_t_of_xmls opts subs) in
          Some (Channel ch)

let read_sources xmls =
  match get_elt "sources" xmls with
    None -> []
  | Some (_, subs) ->
      let f acc xml =
        match read_source [xml] with
          None -> acc
        | Some s -> s :: acc
      in
      List.rev (List.fold_left f [] subs)
;;

let read_target xmls = read_source ~tag: "target" xmls

let tags_contains =
  [
    tag_type_short ; tag_keywords_short ;
    tag_speakers_short ; tag_organizers_short ;
  ]

let read_re_items =
  let f acc = function
    E ((("", "item"), atts), [D s]) ->
      let re =
        match get_att "regexp" atts with
          Some "true" -> Str.regexp s
        | _ -> Str.regexp_string s
      in
      re :: acc
  | _ -> acc
  in
  fun cont xmls ->
    cont (List.rev (List.fold_left f [] xmls))
;;

let get_date_bound att atts =
  match get_att att atts with
    None -> None
  | Some s ->
      try Some (Netdate.parse s)
      with _ -> None
;;

let get_date_bounds atts =
  let before = get_date_bound "before" atts in
  let after = get_date_bound "after" atts in
  (after, before)

let rec read_filter acc = function
| D _ -> acc
| E ((("","and"), _), subs) -> (And (read_filters subs)) :: acc
| E ((("","or"), _), subs) -> (Or (read_filters subs)) :: acc
| E ((("","not"), _), subs) -> (Not (And (read_filters subs))) :: acc
| E (((_, tag), atts), subs) when List.mem tag tags_contains ->
    let connector =
      match get_att "connector" atts with
      | None | Some "and" -> Conn_and
      | Some "or" -> Conn_or
      | Some s -> failwith (Printf.sprintf "Invalid filter connector %S" s)
    in
    let items = read_re_items (fun x -> x) subs in
    (Contains (tag, connector, items)) :: acc
| E (((_,tag), atts), _) when tag = tag_start_short ->
    let (after, before) = get_date_bounds atts in
    (StartDate (after, before)) :: acc
| E (((_,tag), atts), _) when tag = tag_end_short ->
    let (after, before) = get_date_bounds atts in
    (EndDate (after, before)) :: acc
| E (((ns,tag), _), _) ->
    failwith
      (Printf.sprintf "Invalid filter node \"%s%s\""
       (match ns with "" -> "" | s -> s^":") tag
      )

and read_filters xmls = List.fold_left read_filter [] xmls

let read_filter_opt xmls =
  match get_elt "filter" xmls with
    None -> None
  | Some (atts,subs) ->
      let exp = Ers_types.And (read_filters subs) in
      let max =
        match get_att "max" atts with
          None -> None
        | Some s ->
            try Some (int_of_string s)
            with _ -> failwith (s ^ " is not an integer")
      in
      Some { filter_exp = exp ; filter_max = max }

let rec xr_of_xmltree = function
| D str -> XR.cdata str
| E ((tag, atts), subs) ->
    let atts = Xtmpl_rewrite.atts_of_list
      (List.map (fun (name, v) -> (name, Xtmpl_rewrite.from_string v)) atts)
    in
    XR.node tag ~atts (List.map xr_of_xmltree subs)

let read_template_opt xmls =
  match get_elt "template" xmls with
    None -> None
  | Some (atts, subs) ->
      let (tag_atts, other_atts) =
        List.partition (function (("","tag"),_) -> true | _ -> false) atts
      in
      let tag =
        match tag_atts with
          [] -> ("","div")
        | (_, t) :: _ -> ("",t)
      in
      let atts = Xtmpl_rewrite.atts_of_list
        (List.map (fun (name, v) -> (name, [Xtmpl_rewrite.cdata v])) other_atts)
      in
      Some (Xtmpl_rewrite.node tag ~atts (List.map xr_of_xmltree subs))
;;

let query_of_xml xml =
  match xml with
    D _ -> failwith "Invalid XML: PCData"
  | E ((_,atts), subs) ->
      let typ = q_return_type_of_atts atts in
      let sources = read_sources subs in
      let target = read_target subs in
      let filter = read_filter_opt subs in
      let tmpl = read_template_opt subs in
      { q_type = typ ;
        q_sources = sources ;
        q_target = target ;
        q_filter = filter ;
        q_tmpl = tmpl ;
      }
;;

let query_of_file file =
  let ic =
     try open_in file
     with Sys_error s -> failwith s
  in
  try
    let xml = Rss.xml_of_source (`Channel ic) in
    close_in ic;
    query_of_xml xml
  with
    e -> close_in ic; raise e
;;

let query_of_string s =
  let xml = Rss.xml_of_source (`String (0, s)) in
  query_of_xml xml
;;

(** {2 Writing} *)

let string_item_xmls = List.map (fun s -> E ((("","item"),[]), [D s]));;

let xmls_of_link ev =
  match ev.ev_link with
    None -> []
  | Some url -> [E((tag_link, []), [ D (Ers_types.string_of_url url) ])]
;;

let xmls_of_level ev = []
let xmls_of_type ev = []
let xmls_of_keywords ev =
  match ev.ev_keywords with
    [] -> []
  | l -> [ E ((tag_keywords, []), string_item_xmls l) ]

let xmls_of_speakers ev =
  match ev.ev_speakers with
    [] -> []
  | l -> [ E ((tag_speakers, []), string_item_xmls l) ]

let xmls_of_organizers ev =
  match ev.ev_organizers with
    [] -> []
  | l -> [ E ((tag_organizers, []), string_item_xmls l) ]

let xmls_of_location ev = []
let xmls_of_start ev = []
let xmls_of_end ev = []
let xmls_of_audience ev = []

let printers =
  [ xmls_of_link ;
    xmls_of_level ;
    xmls_of_type ;
    xmls_of_keywords ;
    xmls_of_speakers ;
    xmls_of_organizers ;
    xmls_of_location ;
    xmls_of_start ;
    xmls_of_end ;
    xmls_of_audience ;
  ]

let item_data_printer ev =
  List.fold_right (fun f acc -> (f ev) @ acc) printers []

let file_of_channel ch file = Rss.print_file ~item_data_printer file ch
let string_of_channel ?indent ch =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  let ch =
    let url = Ers_types.string_of_url Ers_types.base_url in
    try ignore(List.find (fun (_,s) -> s = url) ch.Rss.ch_namespaces); ch
    with Not_found -> { ch with Rss.ch_namespaces = ("ev", url) :: ch.ch_namespaces }
  in
  Rss.print_channel ~item_data_printer ?indent fmt ch ;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

