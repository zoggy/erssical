(** Fetching URLs with CURL. *)

(** @raise Failure in case of error. *)
val get : Neturl.url -> string
