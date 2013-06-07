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

let map_opt f = function None -> "" | Some x -> f x;;
let s_opt = function None -> "" | Some s -> s;;

let tree_opt = function
  None -> Xtmpl.D ""
| Some s ->
  try Xtmpl.xml_of_string s
  with _ -> Xtmpl.D s
;;

let on_source_opt f = function
  None -> Xtmpl.D ""
| Some s -> Xtmpl.D (f s)
;;

let on_data_opt f = function
  None -> Xtmpl.D ""
| Some d -> f d
;;

let env_of_item item =
  let mk env (tag, xml) = Xtmpl.env_add tag (fun _ _ _ -> [xml]) env in
  List.fold_left mk Xtmpl.env_empty
    [
      "item-title", Xtmpl.D (s_opt item.item_title);
      "item-description", Xtmpl.D (s_opt item.item_desc) ;
      "item-url", Xtmpl.D (map_opt Ers_types.string_of_url item.item_link) ;
      "item-pubdate", Xtmpl.D (map_opt Rss.string_of_date item.item_pubdate) ;
      "item-pub-year", Xtmpl.D (map_opt (fun d -> string_of_int d.Netdate.year) item.item_pubdate) ;
      "item-pub-month", Xtmpl.D (map_opt (fun d -> Printf.sprintf "%02d" d.Netdate.month) item.item_pubdate) ;
      "item-pub-mday", Xtmpl.D (map_opt (fun d -> Printf.sprintf "%02d" d.Netdate.day) item.item_pubdate) ;
      "item-pub-hour", Xtmpl.D (map_opt (fun d -> Printf.sprintf "%02d" d.Netdate.hour) item.item_pubdate) ;
      "item-pub-minute", Xtmpl.D (map_opt (fun d -> Printf.sprintf "%02d" d.Netdate.minute) item.item_pubdate) ;
      "item-pub-second", Xtmpl.D (map_opt (fun d -> Printf.sprintf "%02d" d.Netdate.second) item.item_pubdate) ;
      "item-author", Xtmpl.D (s_opt item.item_author) ;
      "item-source-url", on_source_opt (fun s -> Ers_types.string_of_url s.src_url) item.item_source ;
      "item-source-name", on_source_opt (fun s -> s.src_name) item.item_source ;
      "item-ev-keywords", on_data_opt (fun d -> Xtmpl.D (String.concat ", " d.ev_keywords)) item.item_data ;
    ]

let apply_template tmpl channel =
  match tmpl with
    Xtmpl.E (tag, atts, item_tmpl) ->
      let f item =
        let env = env_of_item item in
        Xtmpl.apply_to_xmls env item_tmpl
      in
      let xmls = List.map f channel.ch_items in
      Xtmpl.E (tag, atts, List.flatten xmls)
  | Xtmpl.D _ -> assert false
;;