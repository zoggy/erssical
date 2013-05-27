(******************************************************************************)
(*               Erssical                                                     *)
(*                                                                            *)
(*   Copyright (C) 2013 Institut National de Recherche en Informatique        *)
(*   et en Automatique. All rights reserved.                                  *)
(*                                                                            *)
(*   This program is free software; you can redistribute it and/or modify     *)
(*   it under the terms of the GNU Lesser General Public License version      *)
(*   3 as published by the Free Software Foundation.                          *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU Library General Public License for more details.                     *)
(*                                                                            *)
(*   You should have received a copy of the GNU Library General Public        *)
(*   License along with this program; if not, write to the Free Software      *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                 *)
(*   02111-1307  USA                                                          *)
(*                                                                            *)
(*   Contact: Maxence.Guesdon@inria.fr                                        *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

(** *)

(** Code taken from Easy_engine example of
  {{:https://godirepo.camlcity.org/wwwsvn/trunk/code/?root=lib-ocamlnet2}OCamlnet distribution}
  and adapted to Erssical. *)

open Ers_types;;

type param =
  {
    cache : Ers_cache.t option ;
    log : Ers_log.t option ;
    auth : Ers_auth.t option ;
  }

let log param s =
  match param.log with
    None -> ()
  | Some log -> Ers_log.print log s
;;

(** Keep only HTTP and HTTPS URLs. *)
let filter_urls query =
  let pred url =
    match Neturl.url_scheme url with
      "http" | "https" -> true
    | _ -> false
  in
  let pred_source = function
    Ers_types.Url (url, _) -> pred url
  | Ers_types.Channel _ -> true
  in
  let sources = List.filter pred_source query.q_sources in
  let query = { query with q_sources = sources } in
  match query.q_target with
  | None -> query
  | Some s when pred_source s -> query
  | _ -> { query with q_target = None }
;;

let logstring_of_source = function
  Ers_types.Url (url, _) -> Ers_types.string_of_url url
| Ers_types.Channel _ -> "<inline channel>"
;;

let log_query log client origin ?rtype q =
 let b = Buffer.create 256 in
 Buffer.add_string b ("From "^client^"\n");
 Buffer.add_string b (" Query: "^origin^"\n");
 Buffer.add_string b (" Sources:\n");
 List.iter
   (fun s -> Buffer.add_string b ("  "^(logstring_of_source s)^"\n"))
   q.q_sources;
 (
   match q.q_target with
     None -> ()
   | Some s -> Buffer.add_string b (" Target: "^(logstring_of_source s)^"\n")
  );
 Buffer.add_string b (" Filter: "^(match q.q_filter with None -> "no" | _ -> "yes")^"\n");
 let t =
   let rtype = match rtype with None -> q.q_type | Some t -> t in
   match rtype with
      Ical -> Ers_io.mime_type_ical
    | Rss -> Ers_io.mime_type_rss
    | Xtmpl -> Ers_io.mime_type_xml
    | Debug -> "debug"
  in
 Buffer.add_string b (" Return type: "^t);
 Ers_log.print log (Buffer.contents b)
;;

exception Http_error of string * Nethttp.http_status

let handle_http_query param (cgi : Netcgi.cgi_activation) =
  try
    let env = cgi#environment in
    let client = Printf.sprintf "host %S (addr %S)"
      env#cgi_remote_host env#cgi_remote_addr
    in
    let (query, query_origin) =
      match cgi#argument_value "query-url" with
      | "" when param.auth <> None ->
          raise
            (Http_error ("Inline queries are not allowed by this server. Please provide a query-url argument.", `Unauthorized))
      | "" ->
          begin
            match cgi#argument_value "query" with
              "" -> raise (Http_error ("Missing query or query-url", `Bad_request))
            | s ->
                let q = Ers_io.query_of_string s in
                (q, "<inline <query>")
          end
      | url_s ->
        let url = Ers_types.url_of_string url_s in
        match Neturl.url_scheme url with
            "http" | "https" ->
              begin
                match param.auth with
                  Some auth when not (Ers_auth.url_auth auth url) ->
                  let msg = "Unauthorized query URL: "^(Ers_types.string_of_url url) in
                  raise (Http_error (msg, `Unauthorized))
                | _ -> ()
              end;
              let query_s = Ers_curl.get url in
              let query = Ers_io.query_of_string query_s in
              let origin = Ers_types.string_of_url url in
              (query, origin)
          | _ ->
              let msg = "No valid HTTP or HTTPS URL given" in
              raise (Http_error (msg, `Bad_request))
    in
    let query = filter_urls query in
    let rtype =
      match cgi#argument_value "rtype" with
      | "" -> None
      | "ical" -> Some Ers_types.Ical
      | s when s = Ers_io.mime_type_ical -> Some Ers_types.Ical
      | "xml" -> Some Ers_types.Xtmpl
      | s when s = Ers_io.mime_type_xml -> Some Ers_types.Xtmpl
      | "debug" -> Some Ers_types.Debug
      | _ -> Some Ers_types.Rss
    in
    begin
      match param.log with
        None -> ()
      | Some log -> log_query log client query_origin ?rtype query
    end;
    let (ctype, res) =
      match Ers_do.execute ?cache: param.cache ?rtype query with
        Ers_types.Res_ical s -> (Ers_io.mime_type_ical, s)
      | Ers_types.Res_channel ch -> (Ers_io.mime_type_rss, Ers_io.string_of_channel ch)
      | Ers_types.Res_debug s -> ("text", s)
      | Ers_types.Res_xtmpl tree -> (Ers_io.mime_type_xml, Xtmpl.string_of_xml tree)
    in
    cgi#set_header
      ~cache:`No_cache
      ~content_type:(ctype ^ "; charset=\"UTF-8\"")
      ~fields: ["Access-Control-Allow-Origin", ["*"]]
      ();
    cgi#output#output_string res;
    cgi#output#commit_work ();
    cgi#output#close_out ()
  with
    e ->
      let (msg, status) =
        match e with
          Failure msg -> (msg, `Internal_server_error)
        | Http_error (msg, status) -> (msg, status)
        | _ -> (Printexc.to_string e, `Internal_server_error)
      in
      try
        (* A Netcgi-based content provider *)
        cgi#set_header
          ~status
          ~cache:`No_cache
          ~content_type:"text; charset=\"UTF-8\""
          ~fields: ["Access-Control-Allow-Origin", ["*"]]
          ();
        cgi#output#output_string msg;
        log param msg;
        cgi#output#commit_work ();
        cgi#output#close_out ()
      with
        e ->
          cgi#output#close_out ();
          raise e
;;

let on_request param notification =
  (* This function is called when the full HTTP request has been received. For
   * simplicity, we create a [std_activation] to serve the request.
   *
   * An advanced implementation could set up further notifications to get informed
   * whenever there is space in the response buffer for additional output.
   * Currently, data is fully buffered (first
   * in the transactional buffer, then in the response buffer), and only when
   * the message is complete, the transmission to the client starts.
   * By generating only the next part of the response when there is space in
   * the response buffer, the advanced implementation can prevent that the
   * buffers become large.
   *)
  (*print_endline "Received HTTP request";*)
  ( try
      let env = notification#environment in
      let cgi =
       Netcgi_common.cgi_with_args
         (new Netcgi_common.cgi)
         (env :> Netcgi.cgi_environment)
         Netcgi.buffered_transactional_outtype
         env#input_channel
         (fun _ _ _ -> `Automatic)
     in
     handle_http_query param cgi;
    with
     e ->
       print_endline ("Uncaught exception: " ^ (Printexc.to_string e))
  );
  notification#schedule_finish ()
;;

let on_request_header param (notification : Nethttpd_engine.http_request_header_notification) =
  (* After receiving the HTTP header: We always decide to accept the HTTP body, if any
   * is following. We do not set up special processing of this body, it is just
   * buffered until complete. Then [on_request] will be called.
   *
   * An advanced server could set up a further notification for the HTTP body. This
   * additional function would be called whenever new body data arrives. (Do so by
   * calling [notification # environment # input_ch_async # request_notification].)
   *)
  (*print_endline "Received HTTP header";*)
  let on_request = on_request param in
  notification#schedule_accept_body ~on_request ()
;;

let serve_connection param ues fd =
  (* Creates the http engine for the connection [fd]. When a HTTP header is received
   * the function [on_request_header] is called.
   *)
  let config = Nethttpd_engine.default_http_engine_config in
  Unix.set_nonblock fd;
  let _http_engine =
    let on_request_header = on_request_header param in
    new Nethttpd_engine.http_engine ~on_request_header () config fd ues in
  ()
;;
let rec accept param ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc#accept () in

  let param =
    match param.auth with
      None -> param
    | Some auth -> { param with auth = Some (Ers_auth.read_if_mod auth) }
  in

  Uq_engines.when_state
    ~is_done:(fun (fd, fd_spec) ->
       if srv_sock_acc#multiple_connections then
         (
          serve_connection param ues fd;
          accept param ues srv_sock_acc
         )
       else
         srv_sock_acc#shut_down ()
    )
    ~is_error:(fun _ -> srv_sock_acc#shut_down())
    acc_engine
;;

let start_server param ?(host=Unix.inet_addr_any) ~pending ~port =
  (* We set up [lstn_engine] whose only purpose is to create a server socket listening
   * on the specified port. When the socket is set up, [accept] is called.
   *)
  let ues = Unixqueue.create_unix_event_system () in
  (* Unixqueue.set_debug_mode true; *)
  let opts =
    { (*Uq_engines.default_listen_options with*)
      Uq_engines.lstn_backlog = pending ;
      Uq_engines.lstn_reuseaddr = true ;
    }
  in
  let lstn_engine =
    Uq_engines.listener
      (`Socket(`Sock_inet(Unix.SOCK_STREAM, host, port), opts)) ues
  in
  Uq_engines.when_state ~is_done:(accept param ues) lstn_engine;
  (* Start the main event loop. *)
  Unixqueue.run ues
;;

let inet_addr_of_name host =
  try
    (Unix.gethostbyname host).Unix.h_addr_list.(0)
  with _ ->
      try
        Unix.inet_addr_of_string host
      with _ ->
          let message =
            Printf.sprintf "inet_addr_of_name %s : unknown host" host
          in
          raise (Failure message)
;;

let main () =
  let port = ref 8915 in
  let host = ref None in
  let pending = ref 20 in
  let cache_dir = ref None in
  let auth_file = ref None in
  let log_file = ref None in
  let options =
    [
      "-p", Arg.Set_int port,
      "<n> Listen to port number <n>; default is "^(string_of_int !port) ;

      "-h", Arg.String (fun s -> host := Some s),
      "<name> Reply to connections on host <name>; default is to reply\n\t\tto any query; (\"localhost\" can be used as <name>)" ;

      "-q", Arg.Set_int pending,
      "<n> Set maximum number of pending connections; default is "^ (string_of_int !pending) ;

      "--cache", Arg.String (fun s -> cache_dir := Some s),
      "<dir> Cache fetched RSS channels in <dir>" ;

      "--ttl", Arg.Set_int Ers_cache.default_ttl,
      "<n> When using cache, set default time to live to <n> minutes;\n\t\tdefault is "^
        (string_of_int !Ers_cache.default_ttl);

      "--auth", Arg.String (fun s -> auth_file := Some s),
      "<file> read authorized query urls from <file>" ;

      "--log", Arg.String (fun s -> log_file := Some s),
      "<file> log to <file>" ;
    ]
  in
  let options = Arg.align options in
  Arg.parse options (fun _ -> ())
     (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  let cache =
    match !cache_dir with
      None -> None
    | Some dir -> Some (Ers_cache.mk_cache dir)
  in
  let auth =
    match !auth_file with
      None -> None
    | Some file -> Some (Ers_auth.read_auth file)
  in
  let log =
    match !log_file with
      None -> None
    | Some file -> Some (Ers_log.mk_log file)
  in
  Netsys_signal.init();
  let host =
    match !host with
      None -> None
    | Some "localhost" -> Some Unix.inet_addr_loopback
    | Some h -> Some (inet_addr_of_name h)
  in
  let param = { cache ; log ; auth } in
  begin
    match log with
      None -> ()
    | Some log -> Pervasives.at_exit (fun () -> Ers_log.close log)
  end;
  let rec iter () =
    try start_server param ?host ~pending: !pending ~port: !port
    with e ->
      prerr_endline ("start_server raised "^(Printexc.to_string e));
      iter ()
  in
  iter ()
;;

try main ()
with
  Sys_error msg
| Failure msg ->
    prerr_endline msg; exit 1
| e ->
    prerr_endline (Printexc.to_string e);
    exit 1
;;
