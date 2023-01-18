module M = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  open Lwt.Infix

  type message = { id : int; code : string; body : string } [@@deriving yojson]

  let message =
    let encode { id; code; body } = Ok (id, code, body) in
    let decode (id, code, body) = Ok { id; code; body } in
    let rep = tup3 int string string in
    custom ~encode ~decode rep
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  open Lwt.Infix

  let insert_message body (module Db : Caqti_lwt.CONNECTION) =
    let%lwt result =
      Db.find
        ((string ->! M.message)
       @@ "insert into message (body) values (?) returning *")
        body
    in
    Caqti_lwt.or_fail result

  let delete_message code (module Db : Caqti_lwt.CONNECTION) =
    let%lwt result =
      Db.find
        ((string ->! M.message)
       @@ "delete from message where code = ? returning *")
        code
    in
    Caqti_lwt.or_fail result

  let delete_all_messages (module Db : Caqti_lwt.CONNECTION) =
    let%lwt result =
      Db.collect_list
        ((unit ->* M.message) @@ "delete from message returning *")
        ()
    in
    Caqti_lwt.or_fail result
end

type command_payload = { text : string } [@@deriving yojson]

let command_text request =
  let%lwt body = Dream.body request in
  let payload = body |> Yojson.Safe.from_string |> command_payload_of_yojson in
  Lwt.return payload.text

let tell request =
  let%lwt text = command_text request in
  let%lwt message = Dream.sql request (Q.insert_message text) in
  let display_id = Printf.sprintf "%04d" (message.id mod 10000) in
  Dream.json
    (Printf.sprintf
       "Received!\n\
        Preview: `#%s` %s\n\
        P.S. You can remove this post with `/untellchima #%s %s`" display_id
       message.body display_id message.code)

let untell request =
  let%lwt text = command_text request in
  let code = Scanf.sscanf text "#%4s %8s" (fun _ c -> c) in
  let%lwt _ = Dream.sql request (Q.delete_message code) in
  Dream.json "untellchima successfully"

let flush request =
  let%lwt messages = Dream.sql request Q.delete_all_messages in
  let json = `List (List.map M.yojson_of_message messages) in
  Dream.json (Yojson.Safe.to_string json)

let () =
  Dream.run ~port:8080 @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:database.sqlite3"
  @@ Dream.router
       [
         Dream.post "/tell" tell;
         Dream.post "/untell" untell;
         Dream.post "/flush" flush;
         Dream.post "/echo" (fun request ->
             let%lwt body = Dream.body request in
             Dream.log "%s" body;
             Dream.json body);
       ]
