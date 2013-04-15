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

type t = { file : string ; oc : out_channel }

let mk_log file =
  let oc = open_out_gen [ Open_append ; Open_creat ; Open_text ] 0o600 file in
  { file ; oc }
;;

let date_format = "%d %b %Y %T %z" ;;
let format_date = Netdate.format ~fmt:date_format;;

let date () =
  let t = Unix.time () in
  let date = Netdate.create t in
  format_date date
;;

let print log s =
  output_string log.oc ("["^(date())^"] "^s^"\n");
  flush log.oc
;;

let close log = close_out log.oc;;
  