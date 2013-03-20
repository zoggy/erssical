
(** *)

type event_level = Beginner | Confirmed | Expert
type event_type = Conference | Seminar | Course | Workshop

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
    ev_audience : string ;
  }


type 'a constr = { c_with : 'a list ; c_witho : 'a list }


type filter = {
    f_level : event_level constr ;
    f_tech : string constr ;
    f_type : event_type constr ;
    f_scidom : string constr ;
  }


type feed =
  { feed_sources : Neturl.url list ; (** list of source RSS feeds *)
    feed_filter : filter ;
  }

type item = event Rss.item_t
type channel = (unit, event) Rss.channel_t