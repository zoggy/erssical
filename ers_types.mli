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

(** Representation of Event RSS channels and queries. *)

module SMap : Map.S with type key = string

(** {2 Using URLs} *)

(** @raise Failure in case of parse error. *)
val url_of_string : string -> Uri.t

val string_of_url : Uri.t -> string
val string_of_neturl : Neturl.url -> string

val neturl_of_url : Uri.t -> Neturl.url
val url_of_neturl : Neturl.url -> Uri.t

val compare_url : Uri.t -> Uri.t -> int

(** The base URL used for namespace of event fields in RSS channels. *)
val base_url : Uri.t

(** {2 Events}

An event is represented by additional information in nodes under the [<item>]
 XML nodes. These additional nodes are prefixed by a namespace (to be valid
 RSS extension). This namespace must be associated to the {!base_url} above.
*)

type event_level = Beginner | Confirmed | Expert
type event_type = Conference | Seminar | Course | Workshop | Dojo | Other_type of string
type location = { loc_href : Uri.t option; loc_name : string; }
type event = {
  ev_link : Uri.t option ;
  ev_level : event_level option;
  ev_type : event_type option;
  ev_keywords : string list;
  ev_speakers : string list;
  ev_organizers : string list;
  ev_location : location option;
  ev_start : Netdate.t option;
  ev_end : Netdate.t option;
  ev_audience : string option;
}

val event :
  ?link: Uri.t ->
  ?level:event_level ->
  ?typ:event_type ->
  ?keywords:string list ->
  ?speakers:string list ->
  ?organizers:string list ->
  ?location:location ->
  ?start_date:Netdate.t ->
  ?end_date:Netdate.t -> ?audience:string -> unit -> event

(** {2 Items and channels} *)

type item = event Rss.item_t
type channel = (unit, event) Rss.channel_t
module ItemSet : Set.S with type elt = item

(** {2 Filters}

Filters allow to keep only some items of an Event RSS channel.
*)

type contains_connector = Conn_or | Conn_and
type filter_exp =
    Not of filter_exp
  | Or of filter_exp list
  | And of filter_exp list
  | Contains of (string * contains_connector * Str.regexp list)
  | StartDate of Netdate.t option * Netdate.t option (** (after (inclusive), before (exclusive)) *)
  | EndDate of Netdate.t option * Netdate.t option (** (after (inclusive), before (exclusive)) *)

type filter = {
  filter_exp : filter_exp ;
  filter_max : int option ;
}

(** {2 Queries}

A query is composed of source RSS channels, an optional target RSS channel and
an optional filter.
The sources and the optional target will merged, then the filter will be
be applied on the merge result to keep only selected items.

If the source if indicated with a url, additional event information
can be specified for this source. In this case, this
information is used to complete the event information of each item of
the source channel. A typical example is to add default keywords for all
items of a source channel.

The target channel is used as base channel: all its information (title,
link, description, ...) is kept (except items which are processed as described
above).

If no target channel is given, then the first source is used as base channel.
If no source and no target is given, this is considered as an error.

The structure to compute is indicated by the "type" attribute of the
"<query>" top node of the query XML document. "type" can be:
- ["application/rss+xml"] to indicate that computing the query will return
  a new Event RSS containing (eventually filtered) items,
- ["text/calendar"] to indicate that the merged channel must be converted to
  the Ical format.
*)

(** A source can be a channel given directly in a [<source>] node, or
  it can be a URL, given in the "href" attribute of the [<source>] node.
  In the latter case, the channel will be fetched
  from this URL (using Curl library).
*)
type source = Url of Uri.t * event | Channel of channel

(** A query can be asked to return a new channel, a calendar or debug
  information, i.e. text information, for example errors encountered while
  parsing source and target channels.
*)
type query_return_type = Rss | Ical | Debug | Xtmpl

(** This is the result computed from a query. A calendar is just the
  string in the Ical format. *)
type query_result =
    Res_channel of channel
  | Res_ical of string
  | Res_debug of string
  | Res_xtmpl of Xtmpl_rewrite.tree

type query = {
  q_type : query_return_type;
  q_sources : source list;
  q_target : source option;
  q_filter : filter option;
  q_tmpl : Xtmpl_rewrite.tree option ;
}