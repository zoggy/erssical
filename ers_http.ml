(** *)

(** Code taken from Easy_engine example of
  {{:https://godirepo.camlcity.org/wwwsvn/trunk/code/?root=lib-ocamlnet2}OCamlnet distribution}
  and adapted to Erssical. *)

let generate (cgi : Netcgi.cgi_activation) =
  (* A Netcgi-based content provider *)
  cgi#set_header
    ~cache:`No_cache
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ();
  let data =
    "<html>\n" ^
    "  <head><title>Easy Engine</title></head>\n" ^
    "  <body>\n" ^
    "    <a href='foo'>GET something</a><br>\n" ^
    "    <form method=POST encoding='form-data'>\n" ^
    "      <input type=hidden name=sample value='sample'>\n" ^
    "      <input type=submit value='POST something'>\n" ^
    "    </form>\n" ^
    "  </body>\n" ^
    "</html>" in
  cgi#output#output_string data;
  cgi#output#commit_work ();
;;

let on_request notification =
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
  print_endline "Received HTTP request";
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
     generate cgi;
    with
     e ->
       print_endline ("Uncaught exception: " ^ (Printexc.to_string e))
  );
  notification#schedule_finish ()
;;

let on_request_header (notification : Nethttpd_engine.http_request_header_notification) =
  (* After receiving the HTTP header: We always decide to accept the HTTP body, if any
   * is following. We do not set up special processing of this body, it is just
   * buffered until complete. Then [on_request] will be called.
   *
   * An advanced server could set up a further notification for the HTTP body. This
   * additional function would be called whenever new body data arrives. (Do so by
   * calling [notification # environment # input_ch_async # request_notification].)
   *)
  print_endline "Received HTTP header";
  notification#schedule_accept_body ~on_request ()
;;

let serve_connection ues fd =
  (* Creates the http engine for the connection [fd]. When a HTTP header is received
   * the function [on_request_header] is called.
   *)
  let config = Nethttpd_engine.default_http_engine_config in
  Unix.set_nonblock fd;
  let _http_engine =
    new Nethttpd_engine.http_engine ~on_request_header () config fd ues in
  ()
;;
let rec accept ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc#accept () in
  Uq_engines.when_state
    ~is_done:(fun (fd, fd_spec) ->
       if srv_sock_acc#multiple_connections then
         (
          serve_connection ues fd;
          accept ues srv_sock_acc
         )
       else
         srv_sock_acc#shut_down ()
    )
    ~is_error:(fun _ -> srv_sock_acc#shut_down())
    acc_engine
;;

let start_server ?(host=Unix.inet_addr_any) ~pending ~port =
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
  Uq_engines.when_state ~is_done:(accept ues) lstn_engine;
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
  let options =
    [
      "-p", Arg.Set_int port,
      "<n> Listen to port number <n>; default is "^(string_of_int !port) ;

      "-h", Arg.String (fun s -> host := Some s),
      "<name> reply to connections on host <name>; default is to
      reply to any query; (\"localhost\" can be used as <name>)" ;

      "-q", Arg.Set_int pending,
      "<n> Set maximum number of pending connections; default is "^ (string_of_int !pending) ;
    ]
  in
  Arg.parse options (fun _ -> ())
     (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  Netsys_signal.init();
  let host =
    match !host with
      None -> None
    | Some "localhost" -> Some Unix.inet_addr_loopback
    | Some h -> Some (inet_addr_of_name h)
  in
  start_server ?host ~pending: !pending ~port: !port
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
