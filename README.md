# DynamoDB-OCaml

A small library that enables simple DDB queries.

```ml
open Base
open Dynamodb.Request
open Lwt.Syntax

let () =
  let main =
    let+ { items; _ } = Scan.request "table-name" |> Lwt.map Result.ok_or_failwith in
    List.iter ~f:(fun i -> Yojson.Safe.to_string i |> Stdio.print_endline) items
  in
  Lwt_main.run main
```

## Currently-Implemented APIs

> **Note**
> None of these APIs currently accept all of the available request params
> as defined by the AWS documentation.

- `GetItem`
- `PutItem`
- `DeleteItem`
- `Scan`

