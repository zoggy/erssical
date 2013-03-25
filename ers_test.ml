(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let q = Ers_io.query_of_file Sys.argv.(2) ;;
let channels = Ers_do.get_source_channels q;;
let channel = Ers_do.merge_channels ?target: q.Ers_types.q_target channels;;
let str = Ers_io.string_of_channel ~indent: 2 channel;;
print_string str;;