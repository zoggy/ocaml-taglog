
type tag = Foo | Bar | Gee

let tag_of_yojson = function
| `String "Foo" -> Foo
| `String "Bar" -> Bar
| `String "Gee" -> Gee
| json -> Ocf.invalid_value json

let tag_to_yojson = function
| Foo -> `String "Foo"
| Bar -> `String "Bar"
| Gee -> `String "Gee"

let tag_wrapper =
  let to_ = tag_to_yojson in
  let of_ ?def x = tag_of_yojson x in
  Ocf.Wrapper.make to_ of_

module T = Taglog.Make
  (struct type t = tag let compare = compare end)

let log_options () =
  let option_level = Ocf.int 0 in
  let option_cond = Ocf.option
    (Ocf.Wrapper.option (Taglog.cond_wrapper tag_wrapper)) None
  in
  (option_level, option_cond)

let (dump_level, dump_cond) = log_options ()

let log ?level ?tags str =
  let printed = ref false in
  T.mk_str_log dump_level dump_cond
    (fun s -> printed := true; print_endline s)
    ?level ?tags str ;
  if not !printed then
    failwith ("Failed with test: "^str)

let fail ?level ?tags str =
  T.mk_fmt_log dump_level dump_cond
    (Printf.ksprintf print_endline) ?level ?tags
    "%s %a" str (fun () -> assert false) ()

open Taglog.Operators

let () = Ocf.set dump_level 1

let () =
  let g = Ocf.add Ocf.group ["condition"] dump_cond in
  try
    Ocf.from_string g
      {| { condition: ("OR", "Foo", ("AND", "Bar", ("NOT", "Gee"))) } |};
    print_endline (Ocf.to_string g)
  with
  Ocf.Error e ->
    prerr_endline (Ocf.string_of_error e);
    exit 1

let test_levels () =
  Ocf.set dump_cond None ;
  log ~level: 0 "OK level 0"  ;
  log ~level: 1 "OK level 1" ;
  fail ~level: 2 "level 2" ;
  log "=> test_levels OK"

let test_tags () =
  Ocf.set dump_cond
    (Some Taglog.Operators.(
      ??Foo || ??Bar && (~~ ??Gee)
    ));
  log ~tags: [Foo] "OK with tag Foo" ;
  log ~tags: [Bar] "OK with tag Bar" ;
  log ~tags: [Bar ; Foo] "OK with tags Bar and Foo" ;
  fail ~tags: [Bar ; Gee] "KO with tags Bar and Gee" ;
  fail ~tags: [Gee] "KO with tag Gee" ;
  fail "KO with no tag" ;
  log ~tags: [Foo] "=> test_tags OK"

let () = test_levels ()
let () = test_tags ()
