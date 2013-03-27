(** Operaitons on channels and queries. *)

(** Return the list of channels from the queries, fetching channels from
  URLs if needed.
  @raise Failure in case of error.
  @return a list of pairs [(channel, errors)], where [errors] is the list
  of non-fatal errors encountered while parsing the channel XML.
*)
val get_source_channels :
  Ers_types.query -> (Ers_types.channel * string list) list

(** Same as {!get_source_channel} but return the optional target of the query. *)
val get_target_channel :
  Ers_types.query -> (Ers_types.channel * string list) option

module UMap : Map.S with type key = Neturl.url

(** [merge_channels ?target sources] merges the given source channels,
  including the optional [target] channel.

  If two items have the same [item_link], then only
  the first one is kept. The [item_source] field of each item is set
  to the original channel URL, if the source channel was given with
  an URL in the query. Return a new channel, using information
  from the "base" channel. This "base" channel is the given target,
  or else the first channel of the list.
  @raise Failure in case of error (e.g. when no channel is given).
*)
val merge_channels :
  ?target:('a, 'b) Rss.channel_t ->
  ('a, 'b) Rss.channel_t list -> ('a, 'b) Rss.channel_t

(** Execute a query.
 @param rtype can be used to override the return type specified in the query.
 @raise Failure in case of error.
*)
val execute :
  ?rtype:Ers_types.query_return_type ->
  Ers_types.query -> Ers_types.query_result
