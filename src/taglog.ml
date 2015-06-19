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

module W = Ocf.Wrapper

type 'a cond =
  | Tag of 'a
  | Not of 'a cond
  | Or of 'a cond list
  | And of 'a cond list

let cond_wrapper w =
  let rec to_j = function
    Tag x -> w.W.to_json x
  | Not x -> `Tuple [`String "NOT" ; to_j x]
  | Or l -> `Tuple ((`String "OR") :: List.map to_j l)
  | And l -> `Tuple ((`String "AND") :: List.map to_j l)
  in
  let rec from_j ?def = function
  | `Tuple [`String s ; x]
  | `List [`String s ; x] when String.lowercase s = "not" ->
      Not (from_j x)
  | `Tuple [`String s ; `List l]
  | `Tuple ((`String s) :: l)
  | `List [`String s ; `List l]
  | `List ((`String s) :: l) when String.lowercase s = "or" ->
      Or (List.map from_j l)
  | `Tuple [`String s ; `List l]
  | `Tuple ((`String s) :: l)
  | `List [`String s ; `List l]
  | `List ((`String s) :: l) when String.lowercase s = "and" ->
      And (List.map from_j l)
  | json -> Tag (w.W.from_json json)
  in
  W.make to_j from_j

let rec fold_or eval acc = function
| [] -> acc
| cond :: q ->
    let acc = acc || eval cond in
    acc || fold_or eval acc q

let rec fold_and eval acc = function
| [] -> acc
| cond :: q ->
    let acc = acc && eval cond in
    acc && fold_and eval acc q

module Ops =
  struct
    let (??) t = Tag t
    let (~~) c = Not c
    let (||) c1 c2 = Or [c1 ; c2]
    let (&&) c1 c2 = And [c1; c2]
  end

let ikprintf k x (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf k x
    CamlinternalFormat.End_of_acc fmt

let iprintf fmt = ikprintf (fun x _ -> ignore x) () fmt

module type S =
  sig
    type tag

    val tag_eval : tag list -> tag -> bool
    val eval : (tag list -> tag cond -> bool)

    val extend_tag_eval :
      ((tag list -> tag -> bool) -> tag list -> tag -> bool) -> unit

    val extend_eval :
      ((tag list -> tag cond -> bool) -> tag list -> tag cond -> bool) -> unit

    val mk : ('a -> 'b) ->
        int Ocf.conf_option ->
        tag cond option Ocf.conf_option ->
        (?level:int -> ?tags:tag list -> 'a -> 'b) ->
        ?level:int -> ?tags:tag list -> 'a -> 'b

    val mk_log :
        int Ocf.conf_option ->
        tag cond option Ocf.conf_option ->
        (?level:int -> ?tags:tag list ->
          ('a, unit, 'b, unit) format4 -> 'a) ->
        ?level:int ->
        ?tags:tag list -> ('a, unit, 'b, unit) format4 -> 'a

    val mk_fmt_log :
      int Ocf.conf_option ->
        tag cond option Ocf.conf_option ->
        (('a, unit, 'b, unit) format4 -> 'a) ->
        ?level:int ->
        ?tags:tag list ->
        ('a, unit, 'b, unit) format4 -> 'a

    val mk_str_log :
      int Ocf.conf_option ->
        tag cond option Ocf.conf_option ->
        (string -> unit) ->
        ?level:int -> ?tags:tag list -> string -> unit
  end

module type P = sig type t end

module Make(T:P) : S with type tag = T.t =
  struct
    type tag = T.t
    let ref_tag_eval = ref (fun _ _ -> assert false)
    let ref_eval = ref (fun _ _ -> assert false)

    let extend_tag_eval f = ref_tag_eval := f !ref_tag_eval
    let extend_eval f = ref_eval := f !ref_eval

    let tag_eval list tag = !ref_tag_eval list tag
    let eval list cond = !ref_eval list cond

    let () =
      let f tags = function
      | Tag t -> tag_eval tags t
      | Not c -> not (eval tags c)
      | Or l -> fold_or (eval tags) false l
      | And [] -> false
      | And l -> fold_and (eval tags) true l
      in
      ref_eval := f

    let () =
      let f tags tag = List.mem tag tags in
      ref_tag_eval := f


    let mk ign =
      fun option_level
          (option_cond : tag cond option Ocf.conf_option) f ->
      fun ?level ?tags arg ->
        let lev = match level with None -> 0 | Some n -> n in
        if Ocf.get option_level < lev then
          ign arg
        else
          match Ocf.get option_cond with
          | None -> f ?level ?tags arg
          | Some cond ->
              match tags with
              | Some list when eval list cond -> f ?level ?tags arg
              | _ -> ign arg

    let mk_log o_lev o_cond f = mk iprintf o_lev o_cond f
    let mk_fmt_log o_lev o_cond f = mk iprintf o_lev o_cond
      (fun ?level ?tags x -> f x)
    let mk_str_log o_lev o_cond f =
      mk (ignore : string -> unit) o_lev o_cond
      (fun ?level ?tags x -> f x)

  end

