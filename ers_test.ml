(** *)

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
let q = Ers_io.query_of_file Sys.argv.(2) ;;
let res = Ers_do.execute (*~rtype: Ers_types.Debug*) q;;
match res with
  Ers_types.Res_debug s -> prerr_endline s
| Ers_types.Res_channel channel ->
    let str = Ers_io.string_of_channel ~indent: 2 channel in
    print_string str
| Ers_types.Res_ical str ->
    print_string str
;;