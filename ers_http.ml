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
open Lwt.Infix

type param =
  {
    log : Ers_log.t ;
    auth : Ers_auth.t option ;
  }

let log param s = Ers_log.print param.log s
;;

(** Keep only HTTP and HTTPS URLs. *)
let filter_urls query =
  let pred url =
    match Uri.scheme url with
      Some "http" | Some "https" -> true
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

let log_query log origin ?rtype q =
 let b = Buffer.create 256 in
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

exception Http_error of string * Cohttp.Code.status_code

let uri_arg = Uri.get_query_param

let handler param uri meth headers body =
  try%lwt
    let%lwt (query, query_origin) =
      match uri_arg uri "query-url" with
      | None | Some "" when param.auth <> None ->
          Lwt.fail
            (Http_error ("Inline queries are not allowed by this server. Please provide a query-url argument.", `Unauthorized))
      | None | Some "" ->
          begin
            match uri_arg uri "query" with
              None | Some "" -> Lwt.fail (Http_error ("Missing query or query-url", `Bad_request))
            | Some s ->
                let q = Ers_io.query_of_string s in
                Lwt.return (q, "<inline <query>")
          end
      | Some url_s ->
          let url = Ers_types.url_of_string url_s in
          match Uri.scheme url with
            Some "http" | Some "https" ->
              begin
                match param.auth with
                | Some auth when not (Ers_auth.url_auth auth url) ->
                    let msg = "Unauthorized query URL: "^(Ers_types.string_of_url url) in
                    Lwt.fail (Http_error (msg, `Unauthorized))
                | _ ->
                    let%lwt query_s = Ers_fetch.get param.log url in
                    let query = Ers_io.query_of_string query_s in
                    let origin = Ers_types.string_of_url url in
                    Lwt.return (query, origin)
              end
          | _ ->
              let msg = "No valid HTTP or HTTPS URL given" in
              Lwt.fail (Http_error (msg, `Bad_request))
    in
    let query = filter_urls query in
    let rtype =
      match uri_arg uri "rtype" with
      | None | Some "" -> None
      | Some "ical" -> Some Ers_types.Ical
      | Some s when s = Ers_io.mime_type_ical -> Some Ers_types.Ical
      | Some "xml" -> Some Ers_types.Xtmpl
      | Some s when s = Ers_io.mime_type_xml -> Some Ers_types.Xtmpl
      | Some "debug" -> Some Ers_types.Debug
      | _ -> Some Ers_types.Rss
    in
    log_query param.log query_origin ?rtype query >>= fun () ->

      let%lwt (ctype, res) =
        match%lwt Ers_do.execute param.log ?rtype query with
          Ers_types.Res_ical s -> Lwt.return (Ers_io.mime_type_ical, s)
        | Ers_types.Res_channel ch -> Lwt.return (Ers_io.mime_type_rss, Ers_io.string_of_channel ch)
        | Ers_types.Res_debug s -> Lwt.return ("text", s)
        | Ers_types.Res_xtmpl tree -> Lwt.return (Ers_io.mime_type_xml, Xtmpl_rewrite.to_string [tree])
      in
      let headers =
        let h = Cohttp.Header.init () in
        Cohttp.Header.add_list h
          [ "Content-type", ctype ^ "; charset=\"UTF-8\"" ;
            "Access-Control-Allow-Origin", "*"
          ]
      in
      Lwt.return (`OK, headers, res)
  with
    e ->
      let (msg, status) =
        match e with
          Failure msg -> (msg, `Internal_server_error)
        | Http_error (msg, status) -> (msg, status)
        | _ -> (Printexc.to_string e, `Internal_server_error)
      in
      Ers_log.print param.log msg >>= fun () ->
      let headers =
        let h = Cohttp.Header.init () in
        Cohttp.Header.add_list h
          [ "Content-type", "text ; charset=\"UTF-8\"" ;
            "Access-Control-Allow-Origin", "*"
          ]
      in
      Lwt.return (status, headers, msg)
;;

open Cohttp
open Cohttp_lwt_unix
module S = Cohttp_lwt_unix.Server

let main () =
  let port = ref 8915 in
  let host = ref None in
  let pending = ref 20 in
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

      "--ttl", Arg.Int (fun n -> Ers_fetch.default_ttl := float n),
      "<n> When using cache, set default time to live to <n> minutes;\n\t\tdefault is "^
        (string_of_int (truncate !Ers_fetch.default_ttl));

      "--auth", Arg.String (fun s -> auth_file := Some s),
      "<file> read authorized query urls from <file>" ;

      "--log", Arg.String (fun s -> log_file := Some s),
      "<file> log to <file>" ;
    ]
  in
  let options = Arg.align options in
  Arg.parse options (fun _ -> ())
     (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  let auth =
    match !auth_file with
      None -> None
    | Some file -> Some (Ers_auth.read_auth file)
  in
  let%lwt log =
    match !log_file with
      None -> Lwt.return (Ers_log.stdout ())
    | Some file -> Ers_log.of_file file
  in
  let host =
    match !host with
      None -> "0.0.0.0"
    | Some s -> s
  in
  let param = { log ; auth } in
  Lwt_main.at_exit (fun () -> Ers_log.close log);
  let server =
    let callback _conn req body =
      let uri = req |> Request.uri in
      let meth = req |> Request.meth |> Code.string_of_method in
      let headers = req |> Request.headers |> Header.to_string in
      body |> Cohttp_lwt_body.to_string >>=
        fun body -> handler param uri meth headers body
        >>= (fun (status, headers, body) ->
           Server.respond_string ~status ~headers ~body ())
    in
    let conn_closed (_,id) = () in
    let config = S.make ~callback ~conn_closed () in
    Conduit_lwt_unix.init ~src:host () >>=
      fun ctx ->
        let ctx = Cohttp_lwt_unix_net.init ~ctx () in
        let mode = `TCP (`Port !port) in
        S.create ~ctx ~mode config
  in
  server

let () = ignore(Lwt_main.run (main ()))


