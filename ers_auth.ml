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

type t = {
  file : string ;
  last_read_time : float ;
  auth_list : Str.regexp list ;
  }

let re_sep = Str.regexp_string " //";;

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

let read_auth_lines ic =
  let rec iter acc =
    match
      try Some (input_line ic)
      with End_of_file -> None
    with
      None -> List.rev acc
    | Some line ->
        let pat =
          try
          let p = Str.search_forward re_sep line 0 in
          String.sub line 0 p
          with Not_found -> strip_string line
        in
        iter (Str.regexp pat :: acc)
  in
  iter []
;;

let file_date file =
  try (Unix.stat file).Unix.st_mtime
  with
    Unix.Unix_error (e, s1, s2) ->
      failwith (Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2)
;;

let read_auth file =
  let ic = open_in file in
  let time = file_date file in
  let l = read_auth_lines ic in
  let t = { file = file ; last_read_time = time ; auth_list = l } in
  close_in ic;
  t
;;

let read_if_mod t =
  let mtime =  file_date t.file in
  if mtime > t.last_read_time then
    read_auth t.file
  else
    t
;;

let url_auth t url =
  let url = Ers_types.string_of_url url in
  let pred re = Str.string_match re url 0 in
  List.exists pred t.auth_list
;;

