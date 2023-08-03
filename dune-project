(lang dune 3.6)

(name dynamodb_ocaml)

(generate_opam_files true)

(source
 (github CoBrooks/dynamodb-ocaml))

(authors "Cole Boooks")

(maintainers "Cole Brooks")

(license MIT)

(package
 (name dynamodb_ocaml)
 (synopsis "A minimal library that implements a small subset of DynamoDB's Client API")
 (depends
  ocaml
  dune
  base
  unix
  uri
  digestif
  yojson
  cohttp-lwt-unix
  ppx_deriving_yojson))

