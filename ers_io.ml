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

let read_level ev atts xmls = ev
let read_type ev atts xmls = ev
let dummy ev atts xmls = ev

let funs = [
    tag_level, read_level ;
    tag_type, read_type ;
    tag_tech, dummy ;
    tag_scidom, dummy ;
    tag_speakers, dummy ;
    tag_organizers, dummy ;
    tag_location, dummy ;
    tag_start, dummy ;
    tag_end, dummy ;
    tag_audience, dummy ;
  ]

let read_event_xml ev = function
  E ((tag, atts), xmls) ->
    begin
      try (List.assoc tag funs) ev atts xmls
      with Not_found -> ev
    end
| D _ -> ev

let read_item_data xmls =
  let ev = Ers_types.event () in
  let ev = List.fold_left read_event_xml ev xmls in
  Some ev
;;


let (opts : (unit, Ers_types.event) Rss.opts) = Rss.make_opts ~read_item_data ();;

let channel_of_file file =
  Rss.channel_t_of_file opts file