open Base
open Lwt.Syntax

let secret_key = Sys.getenv_exn "AWS_SECRET_ACCESS_KEY"
and access_key = Sys.getenv_exn "AWS_ACCESS_KEY_ID"

and region =
  Option.first_some (Sys.getenv "AWS_REGION") (Sys.getenv "AWS_DEFAULT_REGION")
  |> Option.value_exn
       ~message:"Missing environment variable AWS_REGION or AWS_DEFAULT_REGION"

module Util = struct
  let ddb_request ?proto operation =
    let uri =
      Printf.sprintf "%s://dynamodb.%s.amazonaws.com"
        (Option.value ~default:"http" proto)
        region
      |> Uri.of_string
    in
    ( `POST,
      uri,
      [
        ("x-amz-target", Printf.sprintf "DynamoDB_20120810.%s" operation);
        ("content-type", "application/json");
      ] )

  let sign_request payload =
    Signing.sign_request ~payload ~service:"dynamodb" ~region ~access_key
      ~secret_key

  let dynamo_prop_to_yojson = function
    | `Assoc [ ("S", `String s) ] -> Ok (`String s)
    | `Assoc [ ("N", `String str) ] ->
        Ok
          (try `Int (Int.of_string str) with _ -> `Float (Float.of_string str))
    | `Assoc [ ("L", `List ss) ] -> Ok (`List ss)
    | _ -> Error "unrecognized or malformed property"

  let yojson_to_dynamo_prop = function
    | `String str -> Ok ("S", `String str)
    | `Int i -> Ok ("N", `String (Int.to_string i))
    | `Float f -> Ok ("N", `String (Float.to_string f))
    | `List l -> Ok ("L", `List l)
    | _ -> Error "Unknown type"
end

module GetItem = struct
  type t = { item : Yojson.Basic.t }

  let of_yojson = function
    | `Assoc [ ("Item", item) ] ->
        Result.map ~f:(fun item -> { item }) (Util.dynamo_prop_to_yojson item)
    | _ -> Error "unrecognized property"

  let request ?proto table_name (key_name, key_value) =
    let json =
      `Assoc
        [
          ("TableName", `String table_name);
          ("Key", `Assoc [ (key_name, `Assoc [ ("S", `String key_value) ]) ]);
        ]
    in
    let payload = Yojson.Basic.to_string json in
    let request = Util.ddb_request ?proto "GetItem" in
    let meth, uri, headers = Util.sign_request payload request in
    let body = Cohttp_lwt.Body.of_string payload in
    let* response, body =
      Cohttp_lwt_unix.Client.call ~body
        ~headers:(Cohttp.Header.of_list headers)
        meth uri
    in
    let* body = Cohttp_lwt.Body.to_string body in
    match response.status with
    | `OK ->
        let item = Yojson.Basic.from_string body |> of_yojson in
        Lwt.return item
    | _ -> Lwt.return_error body
end

module PutItem = struct
  type t = { table_name : string; item : Yojson.Basic.t }

  let to_dynamo_json { table_name; item } =
    let inner (`Assoc obj) =
      let props =
        List.map
          ~f:(fun (key, value) ->
            Util.yojson_to_dynamo_prop value
            |> Result.map ~f:(fun (typ, value) ->
                   `Assoc [ (key, `Assoc [ (typ, value) ]) ]))
          obj
      in
      Result.all props
      |> Result.map ~f:(fun obj ->
             List.fold obj ~init:(`Assoc []) ~f:Yojson.Basic.Util.combine)
    in
    match item with
    | `Assoc _ as obj ->
        Result.map
          ~f:(fun item ->
            `Assoc [ ("TableName", `String table_name); ("Item", item) ])
          (inner obj)
    | _ -> Error "Not a valid object"

  let request ?proto table_name item =
    let payload =
      { table_name; item } |> to_dynamo_json
      |> Result.map ~f:Yojson.Basic.to_string
      |> Result.ok_or_failwith
    in
    let request = Util.ddb_request ?proto "PutItem" in
    let meth, uri, headers = Util.sign_request payload request in
    let body = Cohttp_lwt.Body.of_string payload in
    let* response, body =
      Cohttp_lwt_unix.Client.call ~body
        ~headers:(Cohttp.Header.of_list headers)
        meth uri
    in
    let* body = Cohttp_lwt.Body.to_string body in
    match response.status with
    | `OK ->
        let item = Yojson.Basic.from_string body in
        Lwt.return_ok item
    | _ -> Lwt.return_error body
end

module DeleteItem = struct
  type t = { table_name : string; key : Yojson.Basic.t }

  let to_dynamo_json { table_name; key } =
    let inner (`Assoc obj) =
      let props =
        List.map
          ~f:(fun (key, value) ->
            Util.yojson_to_dynamo_prop value
            |> Result.map ~f:(fun (typ, value) ->
                   `Assoc [ (key, `Assoc [ (typ, value) ]) ]))
          obj
      in
      Result.all props
      |> Result.map ~f:(fun obj ->
             List.fold obj ~init:(`Assoc []) ~f:Yojson.Basic.Util.combine)
    in
    match key with
    | `Assoc _ as obj ->
        Result.map
          ~f:(fun key ->
            `Assoc [ ("TableName", `String table_name); ("Key", key) ])
          (inner obj)
    | _ -> Error "Not a valid object"

  let request ?proto table_name key =
    let payload =
      { table_name; key } |> to_dynamo_json
      |> Result.map ~f:Yojson.Basic.to_string
      |> Result.ok_or_failwith
    in
    let request = Util.ddb_request ?proto "DeleteItem" in
    let meth, uri, headers = Util.sign_request payload request in
    let body = Cohttp_lwt.Body.of_string payload in
    let* response, body =
      Cohttp_lwt_unix.Client.call ~body
        ~headers:(Cohttp.Header.of_list headers)
        meth uri
    in
    let* body = Cohttp_lwt.Body.to_string body in
    match response.status with
    | `OK ->
        let item = Yojson.Basic.from_string body in
        Lwt.return_ok item
    | _ -> Lwt.return_error body
end

module Scan = struct
  type t = { table_name : string }

  let items_of_yojson items =
    let inner = function
      | `Assoc [ (key, `Assoc [ (_, value) ]) ] -> Ok (`Assoc [ (key, value) ])
      | _ -> Error "unknown schema for Items"
    in
    match items with
    | `List items -> List.map ~f:inner items |> Result.all
    | _ -> Error "unknown schema for Items"

  type response = {
    count : int; [@key "Count"]
    items : Yojson.Safe.t list; [@key "Items"] [@of_yojson items_of_yojson]
    scanned_count : int; [@key "ScannedCount"]
  }
  [@@deriving yojson]

  let to_json { table_name } = `Assoc [ ("TableName", `String table_name) ]

  let request ?proto table_name =
    let payload = { table_name } |> to_json |> Yojson.Basic.to_string in
    let request = Util.ddb_request ?proto "Scan" in
    let meth, uri, headers = Util.sign_request payload request in
    let body = Cohttp_lwt.Body.of_string payload in
    let* response, body =
      Cohttp_lwt_unix.Client.call ~body
        ~headers:(Cohttp.Header.of_list headers)
        meth uri
    in
    let* body = Cohttp_lwt.Body.to_string body in
    match response.status with
    | `OK ->
        let item = Yojson.Safe.from_string body |> response_of_yojson in
        Lwt.return item
    | _ -> Lwt.return_error body
end
