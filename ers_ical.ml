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

open Rss
open Ers_types

let print_date buf label t =
  let tz = t.Netdate.zone in
  (* there is a bug Netdate <= 3.6.3, so we apply a workaround *)
  let tz =
    if abs tz >= 100 then
      (tz / 100) * 60 + tz mod 100
    else
      tz
  in
  let t = { t with Netdate.zone = tz } in
  let f = Netdate.since_epoch t in
  let t = Netdate.create ~zone: 0 f in
  Buffer.add_string buf (label^":"^(Netdate.format "%Y%m%dT%H%M%SZ" t)^"\n")

let escape_text s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "\\n"
    | ',' -> Buffer.add_string b "\\,"
    | ';' -> Buffer.add_string b "\\;"
    | '\\' -> Buffer.add_string b "\\\\"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
;;

let print_location_opt buf = function
  None -> ()
| Some loc ->
    Buffer.add_string buf "LOCATION";
      (match loc.loc_href with
         None -> ()
       | Some url ->
           Buffer.add_string buf (";ALTREP=\"" ^ (Ers_types.string_of_url url) ^ "\"");
      );
      Buffer.add_string buf (":" ^ (escape_text loc.loc_name) ^"\n")
;;

let print_description_opt buf = function
  None -> ()
| Some s -> Buffer.add_string buf ("DESCRIPTION:" ^ (escape_text s) ^ "\n")
;;

let print_categories buf ev =
   let cats = List.map escape_text ev.ev_keywords in
   Buffer.add_string buf ("CATEGORIES:" ^ (String.concat "," cats) ^ "\n")
;;

let print_link_opt buf = function
  None -> ()
| Some url -> Buffer.add_string buf ("URL:" ^ (Ers_types.string_of_url url) ^ "\n")
;;

let ical_link_of_item item =
  match item.item_data with
    Some { ev_link = Some url } -> Some url
  | _ -> item.item_link
;;

let print_vevent_of_item buf item =
  let dtstart, dtend =
    let (dtstart, dtend) =
      match item.item_data with
        None -> (None, None)
      | Some ev -> (ev.ev_start, ev.ev_end)
    in
    match dtstart with
      None ->
        begin
          match item.item_pubdate with
            None -> (None, None)
          | Some dt -> (Some dt, None)
        end
    | _ -> (dtstart, dtend)
  in
  match dtstart with
    None -> ()
  | Some dtstart ->
      Buffer.add_string buf "BEGIN:VEVENT\n";
      Buffer.add_string buf "TRANSP:TRANSPARENT\n";
      print_date buf "DTSTART" dtstart;
      begin
        match dtend with
          None -> ()
        | Some dt -> print_date buf "DTEND" dt
      end;
      let summary =
        match item.item_title with
          None -> " "
        | Some s -> String.concat " " (Ers_io.split_string s ['\n' ; '\r'])
      in
      Buffer.add_string buf ("SUMMARY:" ^ summary ^ "\n");
      print_description_opt buf item.item_desc ;
      print_link_opt buf (ical_link_of_item item) ;
      begin
       match item.item_data with
         None -> ()
       | Some ev ->
           print_location_opt buf ev.ev_location;
           print_categories buf ev ;
      end;
      Buffer.add_string buf "END:VEVENT\n"
;;

module USet = Set.Make
  (struct type t = Neturl.url let compare = Ers_types.compare_url end)


let remove_duplicated_events items =
  let rec iter acc seen = function
    [] -> List.rev acc
  | item :: q ->
    match ical_link_of_item item with
      None -> iter (item :: acc) seen q
    | Some url ->
      if USet.mem url seen then
        iter acc seen q
      else
        iter (item :: acc) (USet.add url seen) q
  in
  iter [] USet.empty items
;;

let ical_of_channel ch =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "BEGIN:VCALENDAR\n";
  Buffer.add_string buf "VERSION:2.0\n";
  Buffer.add_string buf "PRODID:-//erssical/1.0\n";
  let items = remove_duplicated_events ch.ch_items in
  List.iter (print_vevent_of_item buf) items;
  Buffer.add_string buf "END:VCALENDAR\n";
  Buffer.contents buf
;;