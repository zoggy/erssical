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

(** Reading and writing channels and queries. *)

(** {2 Tags used in XML event information}
  [tag_short] is the short name, without namespace, for example ["technologies"].
  [tag] is the name with the prefix, which is the {!Ers_types.base_url}.
*)

val tag_level : string * string

val tag_type_short : string
val tag_type : string * string

val tag_tech_short : string
val tag_tech : string * string

val tag_scidom_short : string
val tag_scidom : string * string

val tag_speakers_short : string
val tag_speakers : string * string

val tag_organizers_short : string
val tag_organizers : string * string
val tag_location : string * string

val tag_start_short : string
val tag_start : string * string

val tag_end_short : string
val tag_end : string * string

val tag_audience : string * string

(** {2 Utilities} *)

val split_string : ?keep_empty:bool -> string -> char list -> string list
val strip_string : string -> string
val get_att : ?pref:string -> 'a -> ((string * 'a) * 'b) list -> 'b option
val get_elt :
  ?pref:string ->
  string ->
  Rss.xmltree list -> (Xmlm.attribute list * Rss.xmltree list) option

(** {2 Reading channels and queries from XML}

 The functions raise [Failure] in case of error.
*)

val mime_type_ical : string
val mime_type_rss : string
val opts : (unit, Ers_types.event) Rss.opts

val channel_of_file :
  string -> (unit, Ers_types.event) Rss.channel_t * string list
val channel_of_string :
  string -> (unit, Ers_types.event) Rss.channel_t * string list

val query_of_xml : Rss.xmltree -> Ers_types.query
val query_of_file : string -> Ers_types.query
val query_of_string : string -> Ers_types.query

(** {2 Writing channels and queries to XML} *)

val file_of_channel : ('a, Ers_types.event) Rss.channel_t -> string -> unit
val string_of_channel :
  ?indent:int -> ('a, Ers_types.event) Rss.channel_t -> string
