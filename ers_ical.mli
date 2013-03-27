(** Exporting Event RSS channels to Ical format. *)

val ical_of_channel : ('a, Ers_types.event) Rss.channel_t -> string
