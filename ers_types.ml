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

let url_of_string s =
  try Uri.of_string s
  with _ ->
    failwith (Printf.sprintf "Bad URL %S" s)
;;
let string_of_url = Uri.to_string

let string_of_neturl = Neturl.string_of_url
let compare_url u1 u2 = String.compare (string_of_url u1) (string_of_url u2)

let neturl_of_url t =
  let s = string_of_url t in
  try Neturl.parse_url ~enable_fragment: true ~accept_8bits: true s
  with Neturl.Malformed_URL ->
      failwith (Printf.sprintf "Malformed URL %S" s)

let url_of_neturl t = url_of_string (string_of_neturl t)

let base_url = url_of_string "http://zoggy.github.io/erssical/doc-event.html#"

module SMap = Map.Make
  (struct type t= string let compare = Pervasives.compare end)

type event_level = Beginner | Confirmed | Expert
type event_type = Conference | Seminar | Course | Workshop | Dojo | Other_type of string

type location =
  { loc_href : Uri.t option ;
    loc_name : string ;
  }

type event = {
    ev_link : Uri.t option ;
    ev_level : event_level option ;
    ev_type : event_type option ;
    ev_keywords : string list ;
    ev_speakers : string list ;
    ev_organizers : string list ;
    ev_location : location option ;
    ev_start : Netdate.t option ;
    ev_end : Netdate.t option ;
    ev_audience : string option ;
  }
;;

type item = event Rss.item_t
type channel = (unit, event) Rss.channel_t

module ItemSet = Set.Make
  (struct
     type t = item
     let compare = Rss.compare_item ~comp_data: Pervasives.compare
   end)

type contains_connector = Conn_or | Conn_and
type filter_exp =
| Not of filter_exp
| Or of filter_exp list
| And of filter_exp list
| Contains of (string * contains_connector * Str.regexp list)
| StartDate of Netdate.t option * Netdate.t option
| EndDate of Netdate.t option * Netdate.t option

type comp_filter_exp = ItemSet.t -> ItemSet.t

type filter = {
      filter_exp : filter_exp ;
      filter_max : int option ;
    }
;;

type source = Url of Uri.t * event | Channel of channel

type query_return_type = Rss | Ical | Debug | Xtmpl

type query_result =
  | Res_channel of channel
  | Res_ical of string
  | Res_debug of string
  | Res_xtmpl of Xtmpl_rewrite.tree

type query =
  { q_type : query_return_type ;
    q_sources : source list ; (** list of source RSS feeds *)
    q_target : source option ;
    q_filter : filter option ;
    q_tmpl : Xtmpl_rewrite.tree option ;
  }


let event ?link ?level ?typ ?(keywords=[]) ?(speakers=[])
  ?(organizers=[]) ?location ?start_date ?end_date ?audience () = {
    ev_link = link ;
    ev_level = level ; ev_type = typ ; ev_keywords = keywords ;
    ev_speakers = speakers ; ev_organizers = organizers ;
    ev_location = location ; ev_start = start_date ; ev_end = end_date ;
    ev_audience = audience ;
  }
