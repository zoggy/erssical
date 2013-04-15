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

(** Caching fetched RSS channels. *)

type t

(** Create a cache from the given directory name. Raise [Failure] if the
  cache could not be created. *)
val mk_cache : string -> t

(** The function called to print error messages. Default is {!prerr_endline}. *)
val print_error : (string -> unit) ref

(** Default time to live of cached RSS channels, in minutes.
  Used when a cached RSS channel does no specify a ttl. Default is 60 minutes. *)
val default_ttl : int ref

(** Fetch the given URL of RSS channel, first looking in cache and fetching
  only if time to live has expired. *)
val get : t -> Neturl.url -> string

