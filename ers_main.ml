(******************************************************************************)
(*               Erssical                                                     *)
(*                                                                            *)
(*   Copyright (C) 2013 Institut National de Recherche en Informatique        *)
(*   et en Automatique. All rights reserved.                                  *)
(*                                                                            *)
(*   This program is free software; you can redistribute it and/or modify     *)
(*   it under the terms of the GNU Lesser General Public License version      *)
(*   3 as published by the Free Software Foundation.                          *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU Library General Public License for more details.                     *)
(*                                                                            *)
(*   You should have received a copy of the GNU Library General Public        *)
(*   License along with this program; if not, write to the Free Software      *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                 *)
(*   02111-1307  USA                                                          *)
(*                                                                            *)
(*   Contact: Maxence.Guesdon@inria.fr                                        *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

(** Main module of the [erssical] command line tool.*)

let out_format = ref None;;
let query_url = ref false;;
let cache_dir = ref None;;

let options = [
    "--url", Arg.Set query_url,
    " Indicate that the argument is a url to fetch the query from" ;

    "--ical", Arg.Unit (fun () -> out_format := Some Ers_types.Ical),
    " Output result in Ical format" ;

    "--rss", Arg.Unit (fun () -> out_format := Some Ers_types.Rss),
    " Output result in RSS format" ;

    "--debug", Arg.Unit (fun () -> out_format := Some Ers_types.Debug),
    " Output result in debug format" ;

    "--cache", Arg.String (fun s -> cache_dir := Some s),
    "<dir> Cache fetched RSS channels in <dir>" ;

    "--ttl", Arg.Set_int Ers_cache.default_ttl,
    "<n> Chen using cache, set default time to live to <n> minutes;\n\t\tdefault is "^
    (string_of_int !Ers_cache.default_ttl);
  ]

let usage = Printf.sprintf "%s [options] <url|file>" Sys.argv.(0)

let main () =
  let args = ref [] in
  let options = Arg.align options in
  Arg.parse options (fun s -> args := s :: !args) (usage^"\nwhere options are:");
  match List.rev !args with
    [] | _ :: _ :: _ -> failwith usage
  | [arg] ->
      let cache =
        match !cache_dir with
          None -> None
        | Some dir -> Some (Ers_cache.mk_cache dir)
      in
      let query =
        match !query_url with
          false -> Ers_io.query_of_file arg
        | true ->
            let url = Ers_types.url_of_string arg in
            Ers_io.query_of_string (Ers_curl.get url)
      in
      let res = Ers_do.execute ?cache ?rtype: !out_format query in
      match res with
        Ers_types.Res_debug s -> prerr_endline s
      | Ers_types.Res_channel channel ->
          let str = Ers_io.string_of_channel ~indent: 2 channel in
          print_string str
      | Ers_types.Res_ical str ->
          print_string str
;;

try main ()
with
  Failure msg
| Sys_error msg -> prerr_endline msg; exit 1
| e -> prerr_endline (Printexc.to_string e); exit 1
;;