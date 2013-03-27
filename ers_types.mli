(** Representation of Event RSS channels and queries. *)

module SMap : Map.S with type key = string

(** {2 Using URLs} *)

(** @raise Failure in case of parse error. *)
val url_of_string : string -> Neturl.url

val string_of_url : Neturl.url -> string

val compare_url : Neturl.url -> Neturl.url -> int

(** The base URL used for namespace of event fields in RSS channels. *)
val base_url : Neturl.url

(** {2 Events}

An event is represented by additional information in nodes under the [<item>]
 XML nodes. These additional nodes are prefixed by a namespace (to be valid
 RSS extension). This namespace must be associated to the {!base_url} above.
*)

type event_level = Beginner | Confirmed | Expert
type event_type = Conference | Seminar | Course | Workshop | Dojo | Other_type of string
type location = { loc_href : Neturl.url option; loc_name : string; }
type event = {
  ev_level : event_level option;
  ev_type : event_type option;
  ev_tech : string list;
  ev_scidom : string list;
  ev_speakers : string list;
  ev_organizers : string list;
  ev_location : location option;
  ev_start : Netdate.t option;
  ev_end : Netdate.t option;
  ev_audience : string option;
}

val event :
  ?level:event_level ->
  ?typ:event_type ->
  ?tech:string list ->
  ?scidom:string list ->
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
type filter =
    Not of filter
  | Or of filter list
  | And of filter list
  | Contains of (string * contains_connector * Str.regexp list)
  | StartDate of Netdate.t option * Netdate.t option (** after, before *)
  | EndDate of Netdate.t option * Netdate.t option (** after, before *)

(** {2 Queries}

A query is composed of source RSS channels, an optional target RSS channel and
an optional filter.
The sources and the optional target will merged, then the filter will
be applied on the merge result to keep only selected items.
The target channel is used as base channel: all its information (title,
link, description, ...) is kept (except items which are processed as described
above).
If no target channel is given, then the first source is used as base channel.
If no source and no target is given, this is considered as an error.

The structure to compute is indicated by the "type" attribute of the
"<query>" top node of the query XML document. "type" can be:
- "application/rss+xml" to indicate that computing the query will return
  a new Event RSS containing (eventually filtered) items,
- "text/calendar" to indicate that the merged channel must be converted to
  the Ical format.
*)

(** A source can be a channel given directly in a [<source>] node, or
  it can a URL, given in the "href" attribute of the [<source>] node.
  In the latter case, the channel will be fetch (using Curl library)
  from this URL.
*)
type source = Url of Neturl.url | Channel of channel

(** A query can be asked to return a new channel, a calendar or debug
  information, i.e. text information, for example errors encountered while
  parsing source and target channels.
*)
type query_return_type = Rss | Ical | Debug

(** This is the result computed from a query. A calendar is just the
  string in the Ical format. *)
type query_result =
    Res_channel of channel
  | Res_ical of string
  | Res_debug of string


type query = {
  q_type : query_return_type;
  q_sources : source list;
  q_target : source option;
  q_filter : filter option;
}