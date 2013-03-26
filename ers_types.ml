
(** *)

let url_of_string s =
  try Neturl.parse_url ~enable_fragment: true ~accept_8bits: true s
  with Neturl.Malformed_URL ->
    failwith (Printf.sprintf "Malformed URL %S" s)
;;
let string_of_url = Neturl.string_of_url;;

let compare_url u1 u2 = String.compare (string_of_url u1) (string_of_url u2)

let base_url = url_of_string "http://foo.com/event.html#"

type event_level = Beginner | Confirmed | Expert
type event_type = Conference | Seminar | Course | Workshop | Dojo

type location =
  { loc_href : Neturl.url option ;
    loc_name : string ;
  }

type event = {
    ev_level : event_level option ;
    ev_type : event_type option ;
    ev_tech : string list ;
    ev_scidom : string list ;
    ev_speakers : string list ;
    ev_organizers : string list ;
    ev_location : location option ;
    ev_start : Netdate.t option ;
    ev_end : Netdate.t option ;
    ev_audience : string option ;
  }


type 'a constr = { c_with : 'a list ; c_witho : 'a list }


type filter = {
    f_level : event_level constr ;
    f_tech : string constr ;
    f_type : event_type constr ;
    f_scidom : string constr ;
  }

let empty_constr () = { c_with = [] ; c_witho = [] }

let filter ?(level=empty_constr()) ?(tech=empty_constr())
  ?(typ=empty_constr()) ?(scidom=empty_constr()) () =
  { f_level = level ; f_tech = tech ; f_type = typ ; f_scidom = scidom }
;;

type item = event Rss.item_t
type channel = (unit, event) Rss.channel_t

type source = Url of Neturl.url | Channel of channel

type query_return_type = Rss | Ical | Debug

type query_result =
  | Res_channel of channel
  | Res_ical of string
  | Res_debug of string

type query =
  { q_type : query_return_type ;
    q_sources : source list ; (** list of source RSS feeds *)
    q_target : source option ;
    q_filter : filter ;
  }


let event ?level ?typ ?(tech=[]) ?(scidom=[]) ?(speakers=[])
  ?(organizers=[]) ?location ?start_date ?end_date ?audience () = {
    ev_level = level ; ev_type = typ ; ev_tech = tech ;
    ev_scidom = scidom ; ev_speakers = speakers ; ev_organizers = organizers ;
    ev_location = location ; ev_start = start_date ; ev_end = end_date ;
    ev_audience = audience ;
  }
