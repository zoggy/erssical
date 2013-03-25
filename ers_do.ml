(** *)

open Ers_types;;

let get_source = function
| Feed ch -> ch
| Url url ->
    let contents = Ers_curl.get url in
    fst (Ers_io.channel_of_string contents)

let get_source_channels query =
  List.map get_source query.q_sources

  