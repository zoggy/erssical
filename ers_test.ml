(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let q = Ers_io.query_of_file Sys.argv.(2) ;;
let channels = Ers_do.get_source_channels q;;