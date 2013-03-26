(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let q = Ers_io.query_of_file Sys.argv.(2) ;;
let channels = Ers_do.get_source_channels q;;
let target = Ers_do.get_target_channel q;;
let channel = Ers_do.merge_channels ?target channels;;
let str = Ers_io.string_of_channel ~indent: 2 channel;;
(*print_string str;;*)
let ical = Ers_ical.ical_of_channel channel;;
print_string ical;;