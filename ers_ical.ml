(** *)

open Rss
open Ers_types

let print_date buf label t =
  let t = Netdate.create ~zone: 0 (Netdate.since_epoch t) in
  Buffer.add_string buf (label^":"^(Netdate.format "%Y%m%dT%H%M%SZ" t)^"\n")

let escape_text s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "\\n"
    | ',' -> Buffer.add_string b "\\,"
    | ';' -> Buffer.add_string b "\\;"
    | '\\' -> Buffer.add_string b "\\\\"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
;;

let print_location_opt buf = function
  None -> ()
| Some loc ->
    Buffer.add_string buf "LOCATION";
      (match loc.loc_href with
         None -> ()
       | Some url ->
           Buffer.add_string buf (";ALTREP=\"" ^ (Ers_types.string_of_url url) ^ "\"");
      );
      Buffer.add_string buf (":" ^ (escape_text loc.loc_name) ^"\n")
;;

let print_description_opt buf = function
  None -> ()
| Some s -> Buffer.add_string buf ("DESCRIPTION:" ^ (escape_text s) ^ "\n")
;;

let print_categories buf ev =
   let cats = ev.ev_tech @ ev.ev_scidom in
   let cats = List.map escape_text cats in
   Buffer.add_string buf ("CATEGORIES:" ^ (String.concat "," cats) ^ "\n")
;;

let print_link_opt buf = function
  None -> ()
| Some url -> Buffer.add_string buf ("URL:" ^ (Ers_types.string_of_url url) ^ "\n")
;;

let print_vevent_of_item buf item =
  let dtstart, dtend =
    let (dtstart, dtend) =
      match item.item_data with
        None -> (None, None)
      | Some ev -> (ev.ev_start, ev.ev_end)
    in
    match dtstart with
      None ->
        begin
          match item.item_pubdate with
            None -> (None, None)
          | Some dt -> (Some dt, None)
        end
    | _ -> (dtstart, dtend)
  in
  match dtstart with
    None -> ()
  | Some dtstart ->
      Buffer.add_string buf "BEGIN:VEVENT\n";
      Buffer.add_string buf "TRANSP:TRANSPARENT\n";
      print_date buf "DTSTART" dtstart;
      begin
        match dtend with
          None -> ()
        | Some dt -> print_date buf "DTEND" dt
      end;
      let summary =
        match item.item_title with
          None -> " "
        | Some s -> String.concat " " (Ers_io.split_string s ['\n' ; '\r'])
      in
      Buffer.add_string buf ("SUMMARY:" ^ summary ^ "\n");
      print_description_opt buf item.item_desc ;
      print_link_opt buf item.item_link ;
      begin
       match item.item_data with
         None -> ()
       | Some ev ->
           print_location_opt buf ev.ev_location;
           print_categories buf ev ;
      end;
      Buffer.add_string buf "END:VEVENT\n"
;;

let ical_of_channel ch =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "BEGIN:VCALENDAR\n";
  Buffer.add_string buf "VERSION:2.0\n";
  Buffer.add_string buf "PRODID:-//erssical/1.0\n";
  List.iter (print_vevent_of_item buf) ch.ch_items;
  Buffer.add_string buf "END:VCALENDAR\n";
  Buffer.contents buf
;;