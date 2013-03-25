(** *)

Curl.global_init Curl.CURLINIT_GLOBALALL;;

let writer b data =
  Buffer.add_string b data;
  String.length data
;;

let get url =
  let buf = Buffer.create 256 in
  let connection = Curl.init () in
  Curl.set_url connection (Ers_types.string_of_url url);
  Curl.set_writefunction connection (writer buf);
  Curl.perform connection;
  Curl.cleanup connection;
  Buffer.contents buf
;;