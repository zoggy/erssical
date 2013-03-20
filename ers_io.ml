(** *)

open Rss
open Ers_types

let tag_ =
  let url = Ers_types.string_of_url Ers_types.base_url in
  fun s -> (url, s)

let tag_level = tag_"level"
let tag_type = tag_"type"
let tag_tech = tag_"technologies"
let tag_scidom = tag_"scientificDomains"
let tag_speakers = tag_"speakers"
let tag_organizers = tag_"organizers"
let tag_location = tag_"location"
let tag_start = tag_"startDate"
let tag_end = tag_"endDate"
let tag_audience = tag_"audience"


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

let read_string_items =
  let f acc = function
    E ((("", "item"), _), [D s]) -> s :: acc
  | _ -> acc
  in
  fun cont xmls ->
    cont (List.rev (List.fold_left f [] xmls))
;;

let read_level ev atts xmls = ev
let read_type ev atts xmls = ev

let read_tech ev atts xmls =
  let f l = { ev with ev_tech = ev.ev_tech @ l } in
  read_string_items f xmls

let read_scidom ev atts xmls =
  let f l = { ev with ev_scidom = ev.ev_scidom @ l } in
  read_string_items f xmls

let read_speakers ev atts xmls =
  let f l = { ev with ev_speakers = ev.ev_speakers @ l } in
  read_string_items f xmls

let read_organizers ev atts xmls =
  let f l = { ev with ev_organizers = ev.ev_organizers @ l } in
  read_string_items f xmls

let read_location ev atts xmls = ev

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
    tag_level, read_level ;
    tag_type, read_type ;
    tag_tech, read_tech ;
    tag_scidom, read_scidom ;
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


let (opts : (unit, Ers_types.event) Rss.opts) = Rss.make_opts ~read_item_data ();;

let channel_of_file file = Rss.channel_t_of_file opts file

let string_item_xmls = List.map (fun s -> E ((("","item"),[]), [D s]));;

let xmls_of_level ev = []
let xmls_of_type ev = []
let xmls_of_tech ev =
  match ev.ev_tech with
    [] -> []
  | l -> [ E ((tag_tech, []), string_item_xmls l) ]

let xmls_of_scidom ev =
  match ev.ev_scidom with
    [] -> []
  | l -> [ E ((tag_scidom, []), string_item_xmls l) ]

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
  [ xmls_of_level ;
    xmls_of_type ;
    xmls_of_tech ;
    xmls_of_scidom ;
    xmls_of_speakers ;
    xmls_of_organizers ;
    xmls_of_location ;
    xmls_of_start ;
    xmls_of_end ;
    xmls_of_audience ;
  ]

let item_data_printer ev =
  List.fold_right (fun f acc -> (f ev) @ acc) printers []

let print_file file ch = Rss.print_file ~item_data_printer file ch

module CF = Config_file
let url_wrappers =
  { CF.to_raw = (fun s -> CF.Raw.String (Ers_types.string_of_url s)) ;
    CF.of_raw =
      (function
           CF.Raw.String s -> (Ers_types.url_of_string s)
       | _ -> raise (CF.Wrong_type (fun oc -> output_string oc "Expected a URL"))
      ) ;
  }

let make_group () =
  let group = new CF.group in
  let sources = new CF.list_cp url_wrappers ~group ["sources"] [] "URLs of source feeds" in
  (group, sources)
;;

let feed_of_file file =
  let (g, sources) = make_group () in
  g#read file;
  { feed_sources = sources#get ;
    feed_filter = Ers_types.filter () ;
  }

;;