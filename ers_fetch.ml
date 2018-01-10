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

open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

module I = struct
    type t = Uri.t
    let compare = Uri.compare
    let witness = Uri.of_string "http://foo.net"
  end
module Cache = Lru_cache.Make(I)

type v = {
    mutable timestamp: float ;
    mutable content: Ers_types.channel Lwt.t;
  }
let (cache : v Cache.t) = Cache.init ?validate:None ~size: 100

let default_ttl = ref 60.

let fetch url =
  try%lwt
    Lwt_unix.with_timeout 5.0 (fun () -> Client.get url)
      >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        if code >= 200 && code < 300 then
          body |> Cohttp_lwt_body.to_string
        else
          body |> Cohttp_lwt_body.to_string >>= fun msg ->
            Lwt.fail_with (Printf.sprintf "%s: Error code %d\n%s"
             (Ers_types.string_of_url url) code msg)
  with
    Lwt_unix.Timeout ->
      Lwt.fail_with (Printf.sprintf "%s: Timeout" (Ers_types.string_of_url url))

let get_channel log url =
  let%lwt str = fetch url in
  try
    let (content, errors) = Ers_io.channel_of_string str in
    let s_url = Ers_types.string_of_url url in
    Lwt_list.iter_s
      (fun err -> Ers_log.print log (Printf.sprintf "%s : %s" s_url err))
      errors >>= fun () ->
        Lwt.return content
  with Failure msg -> Lwt.fail_with msg

let get log url = fetch url

let get_channel log =
  let f url =
    let timestamp = Unix.time () in
    let content = get_channel log url in
    { timestamp ; content }
  in
  fun url ->
    let v = Cache.get cache url f in
    let now = Unix.time () in
    if v.timestamp +. !default_ttl *. 60. < now then
      ( v.timestamp <- now ; v.content <- get_channel log url ; v.content )
    else
      v.content
