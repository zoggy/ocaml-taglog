(*********************************************************************************)
(*                Taglog                                                         *)
(*                                                                               *)
(*    Copyright (C) 2015 INRIA. All rights reserved.                             *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License as             *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Logging facilities using tags and levels *)

(** Conditions on tags. *)
type 'a cond =
    Tag of 'a
  | Not of 'a cond
  | Or of 'a cond * 'a cond
  | And of 'a cond * 'a cond

(** [cond_wrapper wrapper] creates a ['a cond] wrapper for
  Ocf options from a wrapper for ['a] values. *)
val cond_wrapper : 'a Ocf.Wrapper.t -> 'a cond Ocf.Wrapper.t

(** Generic evaluation of conditions.
  [eval mem cond] evaluates the condition [cond]. [mem]
  indicates whether a given tag is present. *)
val eval : ('a -> bool) -> 'a cond -> bool

module Operators :
  sig
    (** ?? x => Tag x *)
    val ( ?? ) : 'a -> 'a cond

    (** ~~ x => Not x *)
    val ( ~~ ) : 'a cond -> 'a cond

    (** x || y => Or (x,y) *)
    val ( || ) : 'a cond -> 'a cond -> 'a cond

    (** x && y => And (x,y) *)
    val ( && ) : 'a cond -> 'a cond -> 'a cond

  end

val ikprintf :
  ('b -> ('b, 'c) CamlinternalFormat.acc -> 'd) ->
  'b -> ('a, 'b, 'c, 'd) format4 -> 'a

val iprintf : ('a, unit, 'b, unit) format4 -> 'a

module type S =
  sig
    type tag
    val extend_eval :
      ((tag list -> tag cond -> bool) -> tag list -> tag cond -> bool) ->
      unit
    val mk :
      ('a -> 'b) ->
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (?level:int -> ?tags:tag list -> 'a -> 'b) ->
      ?level:int -> ?tags:tag list -> 'a -> 'b
    val mk_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a) ->
      ?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a
    val mk_fmt_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (('a, unit, 'b, unit) format4 -> 'a) ->
      ?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a
    val mk_str_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (string -> unit) ->
      ?level:int -> ?tags:tag list -> string -> unit
  end
module Make :
  functor (T : Set.OrderedType) -> S with type tag = T.t
