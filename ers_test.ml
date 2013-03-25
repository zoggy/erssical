(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let q = Ers_io.query_of_file Sys.argv.(2) ;;