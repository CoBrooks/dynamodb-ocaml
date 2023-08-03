module Time = struct
  let date_yymmdd Unix.{ tm_year; tm_mon; tm_mday; _ } =
    Printf.sprintf "%04u%02u%02u" (tm_year + 1900) (tm_mon + 1) tm_mday

  let date_time Unix.({ tm_hour; tm_min; tm_sec; _ } as now) =
    date_yymmdd now ^ Printf.sprintf "T%02u%02u%02uZ" tm_hour tm_min tm_sec

  let now_utc () = Unix.time () |> Unix.gmtime
end

module Hash = struct
  let _sha256 ?key str =
    match key with
    | Some key -> Digestif.SHA256.hmac_string ~key str
    | None -> Digestif.SHA256.digest_string str

  let sha256 ?key str = _sha256 ?key str |> Digestif.SHA256.to_raw_string
  let sha256_hex ?key str = _sha256 ?key str |> Digestif.SHA256.to_hex
end

let sign_request ~access_key ~secret_key ~service ~region ~payload
    (meth, uri, headers) =
  let host = Printf.sprintf "%s.%s.amazonaws.com" service region in
  let get_signature_key key date region service =
    let sign msg key = Hash.sha256 ~key msg in
    sign date ("AWS4" ^ key)
    |> sign region |> sign service |> sign "aws4_request"
  in

  let amzdate = Time.date_time @@ Time.now_utc ()
  and datestamp = Time.date_yymmdd @@ Time.now_utc () in

  let encode_query query =
    query
    |> List.map (fun (k, v) -> (k, String.concat "," v))
    |> List.map (fun (k, v) -> (Uri.pct_encode k, Uri.pct_encode v))
    |> List.map (fun (k, v) -> String.concat "=" [ k; v ])
    |> List.sort String.compare |> String.concat "&"
  in

  let canonical_uri = "/"
  and canonical_querystring = encode_query (Uri.query uri)
  and payload_hash = Hash.sha256_hex payload
  and extra_headers =
    headers |> List.map (fun (k, _) -> String.lowercase_ascii k)
  and extra_canon_headers = headers |> List.map (fun (k, v) -> k ^ ":" ^ v) in

  let canonical_headers =
    [ "host:" ^ host; "x-amz-date:" ^ amzdate ] @ extra_canon_headers
    |> List.sort String.compare |> String.concat "\n"
  in
  let signed_headers =
    "host" :: "x-amz-date" :: extra_headers
    |> List.sort String.compare |> String.concat ";"
  in
  let canonical_request =
    [
      "POST";
      canonical_uri;
      canonical_querystring;
      canonical_headers;
      "";
      signed_headers;
      payload_hash;
    ]
    |> String.concat "\n"
  in

  let algorithm = "AWS4-HMAC-SHA256"
  and credential_scope =
    [ datestamp; region; service; "aws4_request" ] |> String.concat "/"
  in

  let string_to_sign =
    [ algorithm; amzdate; credential_scope; Hash.sha256_hex canonical_request ]
    |> String.concat "\n"
  in

  let signing_key = get_signature_key secret_key datestamp region service in
  let signature = Hash.sha256_hex ~key:signing_key string_to_sign in

  let authorization_header =
    Printf.sprintf "%s Credential=%s/%s, SignedHeaders=%s, Signature=%s"
      algorithm access_key credential_scope signed_headers signature
  in
  let headers =
    [ ("x-amz-date", amzdate); ("Authorization", authorization_header) ]
    @ headers
  in
  (meth, uri, headers)
