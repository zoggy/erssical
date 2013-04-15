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

(** *)

type t = string (** a directory *)

let default_ttl = ref 60 (* in minutes *);;

let print_error = ref prerr_endline;;

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let mk_cache dir =
  try
    match Sys.is_directory dir with
      true -> dir
    | false -> failwith (Printf.sprintf "%S exists and is not a directory" dir)
  with
    Sys_error _ ->
      let com = "mkdir -p "^(Filename.quote dir) in
      match Sys.command com with
        0 -> dir
      | n ->
          failwith ("Command failed: "^com)
;;

let get_from_cache dir md5 =
  let file = Filename.concat dir md5 in
  try
    let file_contents = string_of_file file in
    let (ch, _) = Ers_io.channel_of_string file_contents in
    let filedate =
      try (Unix.stat file).Unix.st_mtime
      with _ -> 0.
    in
    let ttl =
      match ch.Rss.ch_ttl with
        None -> !default_ttl
      | Some n -> n
    in
    let curdate = Unix.time () in
    if curdate > filedate +. (float ttl) then
      None
    else
      Some file_contents
  with
    Failure msg
  | Sys_error msg -> !print_error msg; None
;;

let get t url =
  let md5 = Digest.to_hex (Digest.string (Ers_types.string_of_url url)) in
  match get_from_cache t md5 with
    Some contents -> contents
  | None ->
      let contents = Ers_curl.get url in
      let file = Filename.concat t md5 in
      file_of_string ~file contents;
      contents
;;



  