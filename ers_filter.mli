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

(** Filtering items. *)

module S : Set.S with type elt = Ers_types.item

(** This function takes a filter description and returns a function
  to filter a set of items. *)
val compile_filter : Ers_types.filter -> (S.t -> S.t)

(** [filter filt chan] returns a new channel where only items from [ch]
  verifying filter [filt] are kept.*)
val filter :
  Ers_types.filter ->
  ('a, Ers_types.event) Rss.channel_t -> ('a, Ers_types.event) Rss.channel_t
