(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let feed = Ers_io.feed_of_file Sys.argv.(2) ;;