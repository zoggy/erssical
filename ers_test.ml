(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
Ers_io.print_file Sys.argv.(2) ch;;
List.iter prerr_endline errors