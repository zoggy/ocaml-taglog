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

(** {2 General i*printf functions}

  To use when the arguments of a log function must be
  ignored, i.e. they must not trigger computations.
  Example of usage:
  - [ikprintf  (fun _ _ -> Lwt.return_unit) ()] creates a function
    ignoring arguments and returning [unit Lwt.t],
  - [iprintf] is defined as
    [fun fmt -> ikprintf (fun x _ -> ignore x) () fmt].
*)

val ikprintf :
  ('b -> ('b, 'c) CamlinternalFormat.acc -> 'd) ->
  'b -> ('a, 'b, 'c, 'd) format4 -> 'a

val iprintf : ('a, unit, 'b, unit) format4 -> 'a

(** {2 Functorial interface} *)

(** Signature of the module produced by {!Make}. *)
module type S =
  sig
    (** Type of the tags in a condition *)
    type tag

    (** By default, conditions are evaluated using the
         generic {!eval} function.
         [extend_eval f] can be used to use [f] instead of
         the generic evaluation. [f] will be given the previous
         evaluation function stored in the module and replace it.
        [extend_eval] can be used for example when some tags
        have meaningful parameters, or to always return true
        for some tags, like error tags.
    *)
    val extend_eval :
      ((tag list -> tag cond -> bool) -> tag list -> tag cond -> bool) ->
      unit

    (** Main function to create log functions.
      [mk ign level_option cond_option f] returns a new
      function [?level -> ?tags -> fmt -> ...] that we call [g].

      [ign] is the function used to ignore the format arguments.
      [f] is the function called to use the format arguments.
      [f] is given the level and tags passed to [g].
      If no level is given to [g], then it is set to [0].
      When [g] is called, the following conditions are checked:
      - the level is less than or equal to the value stored in
        [level_option],
      - there is no condition stored in [cond_level] or there is
        a condition and it is evaluated to [true] from the given
        list of tags.

      If these two conditions are verified, [f] is called with
      the level, the tags and the format arguments. Else the
      [ign] function is called with the format arguments and
      ignore them.

      Example of usage:
- {[let log ?level =
  mk
    (Taglog.ikprintf (fun _ _ -> Lwt.return_unit) ())
    log_level log_cond fun_log ?level]}
- {!mk_log} is defined as:
  {[let mk_log o_lev o_cond f = mk iprintf o_lev o_cond f]}
           *)
    val mk :
      ('a -> 'b) ->
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (?level:int -> ?tags:tag list -> 'a -> 'b) ->
      ?level:int -> ?tags:tag list -> 'a -> 'b

    (** [mk_log level_option cond_option f] creates
      a log function for formats of type [('a, unit, 'b, unit) format4].*)
    val mk_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a) ->
      ?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a

    (** Same as {!mk_log} but the provided function [f] does not require
      [?level] and [?tags] parameters. *)
    val mk_fmt_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (('a, unit, 'b, unit) format4 -> 'a) ->
      ?level:int -> ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a

    (** Same as {!mk_fmt_log} but creates functions taking a
       string instead of a format argument.
    *)
    val mk_str_log :
      int Ocf.conf_option ->
      tag cond option Ocf.conf_option ->
      (string -> unit) ->
      ?level:int -> ?tags:tag list -> string -> unit
  end

(** Type of module parameter for {!Make}. Type [t] is the
  type of tags.*)
module type P = sig type t end

module Make :
  functor (T : P) -> S with type tag = T.t
