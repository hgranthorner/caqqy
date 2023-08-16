open Base
open Caqti_request.Infix
open Lwt.Infix

let connection_url = "sqlite3:database.db"

let (>>=?) x y = x >>= fun res -> match res with
  | Ok v -> y v
  | Error e -> Error e |> Lwt.return

(* let (>>=!) x y = x >>= fun res -> match res with *)
(*   | Ok v -> y v *)
(*   | Error e -> raise @@ Failure e *)

let unwrap = Caqti_lwt.or_fail

module Db =
  (val Caqti_lwt.connect (Uri.of_string connection_url) >>= Caqti_lwt.or_fail |> Lwt_main.run)

let make_table () =
  let open Caqti_type.Std in
  let query =
    (unit -->. unit)
    @:- {|
          create table if not exists todos (
            id int,
            name text
          )
        |}
  in
  Db.exec query () 

let seed_todos () =
  let open Caqti_type.Std in
  let truncate = (unit -->. unit) @:- "delete from todos" in
  let insert =
    (unit -->. unit)
    @:- {|
          insert into todos (id, name)
          values (1, 'egg'), (2, 'salad')
        |}
  in
  Db.exec truncate () >>=? fun _ ->
  Db.exec insert ()

type todo = { id : int; name : string } [@@deriving show]

let get_todos () =
  let open Caqti_type.Std in
  let query =
    (unit -->* tup2 int string)
    @:- {|
          select *
          from todos
        |}
  in
  Db.fold query
    (fun (id, name) (todos : todo list) -> { id; name } :: todos)
    () []

let main =
  Stdlib.print_endline "";
  let%lwt () = make_table () >>= unwrap in
  let%lwt () = seed_todos () >>= unwrap in
  let%lwt todos = get_todos () >>= unwrap in
  List.iter todos ~f:(fun t -> show_todo t |> Stdlib.print_endline);
  Lwt.return ()

let () = Lwt_main.run main
