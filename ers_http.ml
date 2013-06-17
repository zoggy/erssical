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
  with
    e ->
      let (msg, status) =
        match e with
          Failure msg -> (msg, `Internal_server_error)
        | Http_error (msg, status) -> (msg, status)
        | _ -> (Printexc.to_string e, `Internal_server_error)
      in
      (* A Netcgi-based content provider *)
      cgi#set_header
        ~status
        ~cache:`No_cache
        ~content_type:"text; charset=\"UTF-8\""
        ~fields: ["Access-Control-Allow-Origin", ["*"]]
        ();
      cgi#output#output_string msg;
      log param msg;
;;

let process handler (cgi : Netcgi.cgi_activation) =
  (* The [try] block catches errors during the page generation. *)
  try

    handler cgi;

    (* After the page has been fully generated, we can send it to the
     * browser.
     *)
    cgi#out_channel#commit_work();
  with
    error ->
      (* An error has happened. Generate now an error page instead of
         * the current page. By rolling back the output buffer, any
         * uncomitted material is deleted.
         *)
        cgi#output#rollback_work();

      (* We change the header here only to demonstrate that this is
         * possible.
         *)
      cgi#set_header
      ~status:`Forbidden                  (* Indicate the error *)
      ~cache:`No_cache
      ~content_type:"text/plain; charset=\"utf-8\""
      ();

      cgi#output#output_string "While processing the request an O'Caml exception has been raised:\n";
      let msg =
        match error with
          Failure s | Sys_error s -> s
        | _ -> Printexc.to_string error
      in
      cgi#output#output_string (msg ^ "\n");

      (* Now commit the error page: *)
      cgi#output#commit_work()
;;

let config_tree host port =
  `Section ("netplex",
   [
     `Section ("service",
      [
        `Parameter ("name", `String "nethttpd") ;
        `Section ("protocol",
         [
           `Parameter ("name", `String "http") ;
           `Section ("address",
            [ `Parameter ("type", `String "internet") ;
              `Parameter ("bind", `String (Printf.sprintf "0.0.0.0:%d" port));
            ]) ;
         ]) ;
        `Section ("processor",
         [
           `Parameter ("type", `String "nethttpd") ;
           `Parameter ("access_log", `String "debug");  (* or "off" or "enabled" *)
           `Parameter ("suppress_broken_pipe", `Bool true);
           `Section ("host",
            [
              (* Think of Apache's "virtual hosts" *)
              `Parameter ("pref_name", `String host) ;
              `Parameter ("pref_port", `Int port);
              `Parameter ("names", `String "*:0"); (* Which requests are matched here: all *)
              `Section ("uri",
               [
                 `Parameter ("path", `String "/");
                 `Section ("service",
                  [ `Parameter ("type", `String "dynamic") ;
                    `Parameter ("handler", `String "api");
                  ]);
               ]);
            ]);
         ]);
        `Section ("workload_manager",
         [
           `Parameter ("type", `String "dynamic");
           `Parameter ("max_jobs_per_thread", `Int 1);  (* Everything else is senseless *)
           `Parameter ("min_free_jobs_capacity", `Int 1);
           `Parameter ("max_free_jobs_capacity", `Int 1);
           `Parameter ("max_threads", `Int 20);
         ]);
      ]);
   ]
  )
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
      None -> ""
    | Some s -> s
  in
  let param = { cache ; log ; auth } in
  begin
    match log with
      None -> ()
    | Some log -> Pervasives.at_exit (fun () -> Ers_log.close log)
  end;

  let parallelizer =
    (*Netplex_mt.mt()*)     (* multi-threading *)
    Netplex_mp.mp()   (* multi-processing *)
  in

  let fun_handler _ = process (handle_http_query param) in

  let api =
    { Nethttpd_services.dyn_handler = fun_handler ;
      dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
      dyn_uri = Some "/";                 (* not needed *)
      dyn_translator = (fun s -> s); (* not needed *)
      dyn_accept_all_conditionals = true ;
    }
  in
  let nethttpd_factory =
    Nethttpd_plex.nethttpd_factory
    ~handlers:[ "api", api ]
    ()
  in
  let config_tree = config_tree host !port in
  let netplex_config = Netplex_main.create ~config_tree () in
  let netplex_config = Netplex_main.modify ~foreground: true netplex_config in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory ]           (* make this nethttpd available *)
    netplex_config
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

