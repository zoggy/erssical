(** *)

Curl.global_init Curl.CURLINIT_GLOBALALL;;

let writer b data =
  Buffer.add_string b data;
  String.length data
;;

let handle_curl_error f x =
  try f x
  with Curl.CurlException (curl_code, n, reason) ->
    failwith ("Curl: " ^ reason)

let get url =
  let buf = Buffer.create 256 in
  let connection = handle_curl_error Curl.init () in
  let f () =
    Curl.set_url connection (Ers_types.string_of_url url);
    Curl.set_writefunction connection (writer buf);
    Curl.perform connection;
    Buffer.contents buf
  in
  try handle_curl_error f ()
  with e -> Curl.cleanup connection; raise e
;;