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

Curl.global_init Curl.CURLINIT_GLOBALALL;;

let writer b data =
  Buffer.add_string b data;
  String.length data
;;

let handle_curl_error f x =
  try f x
  with Curl.CurlException (curl_code, n, reason) ->
    failwith ("Curl: " ^ reason)

let get url =
  let buf = Buffer.create 256 in
  let connection = handle_curl_error Curl.init () in
  let f () =
    Curl.set_url connection (Ers_types.string_of_url url);
    Curl.set_writefunction connection (writer buf);
    Curl.perform connection;
    let s = Buffer.contents buf in
    Curl.cleanup connection;
    s
  in
  try handle_curl_error f ()
  with e -> Curl.cleanup connection; raise e
;;