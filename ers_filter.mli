(** Filtering items. *)

module S : Set.S with type elt = Ers_types.item

(** This function takes a filter description and returns a function
  to filter a set of items. *)
val compile_filter : Ers_types.filter -> (S.t -> S.t)

(** [filter filt chan] returns a new channel where only items from [ch]
  verifying filter [filt] are kept.*)
val filter :
  Ers_types.filter ->
  ('a, Ers_types.event) Rss.channel_t -> ('a, Ers_types.event) Rss.channel_t
