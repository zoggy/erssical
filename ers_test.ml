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

let (ch, errors) = Ers_io.channel_of_file Sys.argv.(1);;
print_endline (Ers_io.string_of_channel ch);;
(*
let q = Ers_io.query_of_file Sys.argv.(2) ;;
let res = Ers_do.execute (*~rtype: Ers_types.Ical*) (*~rtype: Ers_types.Debug*) q;;
match res with
  Ers_types.Res_debug s -> prerr_endline s
| Ers_types.Res_channel channel ->
    let str = Ers_io.string_of_channel ~indent: 2 channel in
    print_string str
| Ers_types.Res_ical str ->
    print_string str
;;
*)