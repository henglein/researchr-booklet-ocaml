(* Parser to build ICFP program, by Jacques Garrigue, Jeremy Gibbons, Ranjit Jhala *)
(* Uses Xml-Light *)

open StdLabels
open Printf
open Xml

(******************************************************************************)
type author =
  { first       : string
  ; last        : string
  ; affiliation : string
  }

type event =
  { e_title     : string
  ; e_acronym   : string
  ; e_start     : string
  ; e_end       : string
  ; e_location  : string
  }

type timeslot =
  { t_title   : string
  ; t_start   : string
  ; t_end     : string
  ; t_authors : author list
  ; t_track   : string
  }

type session =
  { s_title : string
  ; s_date  : string
  ; s_room  : string
  ; s_time  : string
  ; s_slots : timeslot list
  }

type page =
  { tr_name   : string
  ; tr_date   : string
  ; tr_title  : string
  ; tr_day    : int option
  }

type subevent =
  { se_name   : string
  ; se_dates  : string list
  ; se_title  : string
  ; se_pos    : int
  }

module SMap = Map.Make(String)

let make_map lst =
  let map = Hashtbl.create 7 in
  List.iter ~f:(fun (key, data) -> Hashtbl.add map key data) lst;
  map
let try_find map s =
  try Hashtbl.find map s with Not_found -> s

(* Configuration *)

(* One page per track and per day *)
(*
let pages = [
  "HOPE 2017", "2017/09/03", "Workshop on Higher-Order Programming with Effects";
  "Programming Languages Mentoring Workshop 2017", "2017/09/03", "Programming Languages Mentoring Workshop";
  "Scheme 2017", "2017/09/03", "Scheme and Functional Programming Workshop";
  "TyDe 2017", "2017/09/03", "Workshop on Type-Driven Development";
  "Tutorials", "2017/09/03", "ICFP Tutorials";
  "ICFP", "2017/09/04", "ICFP -- Day 1";
  "FSCD 2017", "2017/09/04", "FSCD -- Day 1";
  "ICFP", "2017/09/05", "ICFP -- Day 2";
  "FSCD 2017", "2017/09/05", "FSCD -- Day 2";
  "ICFP", "2017/09/06", "ICFP -- Day 3";
  "FSCD 2017", "2017/09/06", "FSCD -- Day 3";
  "FSCD 2017", "2017/09/07", "FSCD -- Day 4";
  "Haskell 2017", "2017/09/07", "Haskell Symposium -- Day 1";
  "ML 2017", "2017/09/07", "ML Family Workshop";
  "Workshop on Functional High-Performance Computing", "2017/09/07", "Workshop on Functional High-Performance Computing";
  "CUFP 2017", "2017/09/07", "CUFP Tutorials -- Day 1";
  "Haskell 2017", "2017/09/08", "Haskell Symposium -- Day 2";
  "OCaml 2017", "2017/09/08", "OCaml Users and Developers Workshop";
  "Erlang 2017", "2017/09/08", "Erlang Workshop";
  "CUFP 2017", "2017/09/08", "CUFP Tutorials -- Day 2";
  "CUFP 2017", "2017/09/09", "Commercial Users of Functional Programming";
  "Haskell Implementors' Workshop", "2017/09/09", "Haskell Implementors Workshop";
  "Workshop on Functional Art, Music, Modeling and Design", "2017/09/09", "Functional Art, Music, Modeling and Design";
]
*)

let subevents : subevent list =
  [ { se_name   = "19th International Conference on Verification, Model Checking, and Abstract Interpretation"
    ; se_dates  = ["2018/01/07"; "2018/01/08"; "2018/01/09"]
    ; se_title  = "VMCAI"
    ; se_pos    = 1
    }
  ; { se_name   = "— ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation"
    ; se_dates  = ["2018/01/08"; "2018/01/09"]
    ; se_title  = "PEPM"
    ; se_pos    = 2
    }
  ; { se_name   = "Research Papers"
    ; se_dates  = ["2018/01/10"; "2018/01/11"; "2018/01/12"]
    ; se_title  = "POPL"
    ; se_pos    = 1
    }
  ; { se_name   = "TutorialFest"
    ; se_dates  = ["2018/01/08"]
    ; se_title  = "TutorialFest"
    ; se_pos    = 5
    }
  (* "POPL: TutorialFest"
  ,  CPP
  ,  PADL
  ,  OBT
  ,  NetPL
  ,  CoqPL
  ,  PLMW
  -- TODO: PPS
  -- TODO: PriSC
  *)
  ]

let subevent_pages (e: subevent) : page list =
  let multi = 2 <= List.length e.se_dates in
  List.mapi
    (fun i d -> { tr_name   = e.se_name
                ; tr_date   = d
                ; tr_title  = e.se_title
                ; tr_day    = if multi then Some (i + 1) else None
                })
    e.se_dates

let pages : page list = List.concat (List.map subevent_pages subevents)

(*
[
  { tr_name  = "19th International Conference on Verification, Model Checking, and Abstract Interpretation"
  ; tr_date  = "2018/01/07"
  ; tr_title = "VMCAI -- Day 1"
  ; tr_short = "Verification, Model Checking, and Abstract Interpretation"
  }
  ]

  "19th International Conference on Verification, Model Checking, and Abstract Interpretation", "2018/01/08", "VMCAI -- Day 2";
  "— ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation", "2018/01/08", "PEPM -- Day 1";
  "20th International Symposium on  Practical Aspects of Declarative Languages ", "2018/01/08", "PADL -- Day 1";
  "CPP 2018 - The 7th ACM SIGPLAN International Conference on Certified Programs and Proofs", "2018/01/08", "CPP -- Day 1";
  "19th International Conference on Verification, Model Checking, and Abstract Interpretation", "2018/01/09", "VMCAI -- Day 3";
  "— ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation", "2018/01/09", "PEPM -- Day 2";
  "20th International Symposium on  Practical Aspects of Declarative Languages ", "2018/01/09", "PADL -- Day 2";
  "CPP 2018 - The 7th ACM SIGPLAN International Conference on Certified Programs and Proofs", "2018/01/09", "CPP -- Day 2";
  "Programming Languages Mentoring Workshop", "2018/01/09", "PLMW";
  "Principles of Secure Compilation", "2018/01/13", "PriSC"
]
*)

let session_chairs =
  let c1 = ref 0 and c2 = ref 0 in
  List.map
    (fun name -> incr c1; sprintf "Keynote Talks: Keynote %d" !c1, name)
    ["Eijiro Sumii"; "Gabriele Keller"; "Jacques Garrigue"] @
  List.map
    (fun name -> incr c2; sprintf "Research Papers: Session %d" !c2, name)
    [ "Akimasa Morihata";
      "Kathleen Fisher";
      "Neel Krishnaswami";
      "Tom Schrijvers";
      "Robby Findler";
      "Johan Jeuring";
      "Andres Löh";
      "Scott Owens";
      "Sam Lindley";
      "John Reppy";
      "Alejandro Russo";
      "Jeremy Gibbons"
    ]

let days = make_map
  [ "2018/01/07", "Sunday"
  ; "2018/01/08", "Monday"
  ; "2018/01/09", "Tuesday"
  ; "2018/01/10", "Wednesday"
  ; "2018/01/11", "Thursday"
  ; "2018/01/12", "Friday"
  ; "2018/01/13", "Saturday"
  ; "",           "Week"
  ]

(* Remapome track names, to allow printing on the same page *)
let track_map = make_map []
(*
    [ "Keynotes and Reports", "ICFP";
      "Student Research Competition", "ICFP";
      "Receptions", "ICFP";
      "Type-driven Development", "TyDe"
      "Research Papers", "ICFP";
      "— ACM SIGPLAN Workshop on Partial Evaluation and Program Manipulation", "PEPM";
      "19th International Conference on Verification, Model Checking, and Abstract Interpretation", "VMCAI"
    ]
*)

let map_track ?(date="") track =
  if track = "Receptions" && date = "2016/09/22" then "Tutorials" else
  try_find track_map track

(* Do not print event with these titles *)
let omit_event = ["Reception"]

(* Night sessions start after *)
let night = "18:15"
let night_track = "Evenings"

(* In these tracks, print the room for each sessions,
   rather than at the top of the page *)
let parallel_sessions = [night_track] (* "Tutorials"; *)

(* Do not print time for this session title *)
let poster_sessions = ["OCaml: Break and Poster Session"]

(* More compact format for these tracks *)
let compact_tracks = ["OCaml"]

(* How to print affiliations, and for which tracks *)
let affiliation_map = make_map []
let map_affiliation = try_find affiliation_map
let use_affiliations = []

(* Generic? code *)

(* Escaping and cleaning *)
let replacements = [
  Str.regexp "ễ", "e";            (* UTF8 characters not supported by TeX *)
  Str.regexp "⇑", "$\\Uparrow$";  (* UTF8 characters not supported by TeX *)
  Str.regexp "λ", "$\\lambda$";   (* UTF8 characters not supported by TeX *)
  Str.regexp "ū", "u";
  Str.regexp "ė", "e";
  Str.regexp "[#%&]", "\\\\\\0";  (* require escaping in TeX *)
]
let protect s =
  List.fold_left ~init:s ~f:(fun s (r, n) -> Str.global_replace r n s)
    replacements

(* Remove track name from session name *)
let track_header = Str.regexp ".*: "
let remove_track s =
  Str.replace_first track_header "" s

(* Do not print session name if it is just a short number *)
let short_number = Str.regexp " *[0-9][0-9]? *"
let omit_session_title s =
  Str.string_match short_number s 0 && Str.match_end () = String.length s

(* The real processing starts here *)

let sort_by ~prj l = List.sort (fun a b -> compare (prj a) (prj b)) l
let fst3 (x,_,_) = x
let snd3 (_,x,_) = x

let hashtbl_inserts k v t =
  let vs = try Hashtbl.find t k with Not_found -> [] in
  Hashtbl.add t k (v::vs)

let hashtbl_to_list t =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) t []

let group kvs =
  let t = Hashtbl.create 7 in
  let _ = List.iter kvs ~f:begin fun (k, v) ->
            let vs = try Hashtbl.find t k with Not_found -> [] in
            Hashtbl.replace t k (v::vs)
          end
  in
  hashtbl_to_list t

(* Parsing *)

let rec map_concat_rev acc ~f = function
    [] -> acc
  | x :: l ->
      map_concat_rev (List.rev_append (f x) acc) ~f l

let map_concat ~f l =
  List.rev (map_concat_rev [] ~f l)

let rec last = function
    [] -> failwith "last"
  | [a] -> a
  | _::l -> last l

let element_contents = function
    Element (_, _, contents) -> contents
  | _ -> []

let is_element ~key = function
    Element (k, _, _) -> k = key
  | PCData _ -> false

let extract_element ~key = function
    Element (k, _, contents) when k = key -> [contents]
  | _ -> []

let find_elements ~key lst =
  map_concat (extract_element ~key) lst

let extract_subevents t =
  find_elements ~key:"subevent" (element_contents t)

let find_str_element ~key ?(default="") lst =
  match find_elements ~key lst with
    [PCData s] :: _ -> s
  | _ -> default

let find_title = find_str_element ~key:"title" ~default:"no title"
let find_date = find_str_element ~key:"date" ~default:"no date"

let sort_by_track subevs =
  let sorted = ref SMap.empty in
  let add_event track ev =
    let date = find_date ev in
    let track = map_track ~date track in
    let evs = try SMap.find track !sorted with Not_found -> [] in
    sorted := SMap.add track (ev :: evs) !sorted
  in
  List.iter subevs ~f:
    begin fun contents ->
      let tracks_ct = List.concat (find_elements ~key:"tracks" contents) in
      let track = find_str_element ~key:"track" tracks_ct in
      let _ = Printf.eprintf "%s ADD-EVENT: %s\n" "%%" track in
      add_event track contents
    end;
  !sorted

let parse_author lst =
  {first = find_str_element ~key:"first_name" lst;
   last  = find_str_element ~key:"last_name" lst;
   affiliation = find_str_element ~key:"affiliation" lst}

let parse_timeslot lst =
  let track =
    find_str_element ~key:"track"
      (List.concat (find_elements ~key:"tracks" lst))
  and t_authors =
    let persons = List.concat (find_elements ~key:"persons" lst) in
    let persons = find_elements ~key:"person" persons in
    List.map ~f:parse_author persons
  in
  let t_start = find_str_element ~key:"start_time" lst
  and t_end   = find_str_element ~key:"end_time" lst in
  (* Hack for HOPE *)
  let t_start =
    if t_start <= "09:10" && t_end >= "09:15" then "09:10" else t_start in
  {t_title = find_title lst;
   t_track = map_track track;
   t_start; t_end; t_authors}


let parse_subevent lst =
  let slots = List.map parse_timeslot (find_elements ~key:"timeslot" lst) in
  let slots = sort_by (fun t -> t.t_start) slots in
  let time =
    if slots = [] then "" else
    sprintf "%s -- %s" (List.hd slots).t_start (last slots).t_end
  in
  {s_title = find_title lst;
   s_date = find_date lst;
   s_room = find_str_element ~key:"room" lst;
   s_time = time; s_slots = slots}

let parse_event_details t =
  let lst = element_contents t in
  let details = List.concat (find_elements ~key:"event_details" lst) in
  let location = List.concat (find_elements ~key:"location" details) in
  {e_title = find_str_element ~key:"title" ~default:"no title" details;
   e_acronym = find_str_element ~key:"acronym" ~default:"no acronym" details;
   e_start = find_str_element ~key:"start_date" details;
   e_end = find_str_element ~key:"end_date" details;
   e_location = find_str_element ~key:"facility_name" location}

(* Printing *)

let output_authors ~track oc lst =
  let lst = List.map lst ~f:
      (fun auth -> {auth with affiliation = map_affiliation auth.affiliation})
  in
  let pr_auth oc au =
    fprintf oc "%s %s" (protect au.first) (protect au.last);
    let affi = au.affiliation in
    if List.mem track use_affiliations && affi <> "" && affi <> "undefined"
    then fprintf oc " (%s)" (protect affi)
  in
  match List.rev lst with
    [] -> ()
  | [au] -> fprintf oc "\\author{%a}\n" pr_auth au
  | au1 :: au2 :: prev ->
      List.iter
        (fun au -> fprintf oc "\\author{%a,}\n" pr_auth au)
        (List.rev prev);
      fprintf oc "\\author{%a and %a}\n" pr_auth au2 pr_auth au1

let output_slot ~track ~poster oc sl =
  let time = if poster then "" else sl.t_start in
  match sl.t_authors with
    (* Hack for a more compact format *)
    [au] when String.length sl.t_title < 20 && List.mem track compact_tracks ->
      fprintf oc "\\slot{%s}{\\makebox[20ex][l]{%s} \\textit{%s %s}}\n"
        time (protect sl.t_title) (protect au.first) (protect au.last)
  | _ ->
      fprintf oc "\\slot{%s}{%s}\n" time (protect sl.t_title);
      if sl.t_authors <> [] && track <> night_track then
        fprintf oc "\\authors{%%\n%a}\n"
          (output_authors ~track) sl.t_authors

let output_session ~track ~room oc ss =
  let chair =
    try List.assoc ss.s_title session_chairs with Not_found -> "" in
  let poster = List.mem ss.s_title poster_sessions in
  let title = protect (remove_track ss.s_title) in
  let title = if omit_session_title title then "" else title in
  let slots =
    List.filter (fun t -> not (List.mem t.t_title omit_event)) ss.s_slots in
  (* Hack to push receptions to bottom *)
  if ss.s_time > night && track <> night_track then fprintf oc "\\vfill\n";
  let title =
    if ss.s_room <> room then title ^ " \\hfill " ^ ss.s_room else
    if chair <> "" then title ^ " \\hfill \\normalfont \\small \\sf " ^ chair
    else title in
  if slots = [] then
    fprintf oc "\\emptysession{%s}{%s}\n" ss.s_time title
  else begin
    fprintf oc "\\session{%s}{%s}\n" ss.s_time title;
    List.iter (fun sl -> output_slot ~track ~poster oc sl) slots;
    fprintf oc "\\closesession\n"
  end

let day_string = function
  | Some i -> " Day -- " ^ string_of_int i
  | None   -> ""

let date_day date =
  let pos = try String.rindex date '/' + 1 with Not_found -> 0 in
  String.sub ~pos ~len:(String.length date - pos) date

let get_sessions track sorted =
    try SMap.find track sorted with Not_found -> []

let output_page page (* ?(date="") ?(title=track) *) oc sessions =
  let date = page.tr_date in
  let title = page.tr_title in
  let sessions =
    if date = "" then sessions else
    List.filter (fun s -> s.s_date = date) sessions
  in
  if sessions = [] then () else
  let sessions =
    sort_by sessions ~prj:(fun s -> s.s_date, s.s_time, s.s_title)
  in
  let first = List.hd sessions in
  let date, multi =
    if date <> "" then (date, false) else
    let last_date = (last sessions).s_date in
    if first.s_date = last_date then (last_date, false) else
    (sprintf "%s -- %s" first.s_date last_date, true)
  in
  let room = if List.mem title parallel_sessions then "" else
             (* CUFP tutorials are in parallel, but the talks are not *)
             if title = "CUFP 2017" && date <> "2017/09/09" then "" else first.s_room in
  fprintf oc "\\header{%s}{%s}{%s}{%s}\n%s"
    title room date  (title ^ day_string page.tr_day)
    (if List.mem title compact_tracks then "\\vspace{-1ex}\n" else "");
  fprintf oc "\\label{%s-%s}\n\n" title (if multi then "" else date_day date);
  let prev_date = ref "" in
  List.iter sessions ~f:
    begin fun ss ->
      if multi && ss.s_date <> !prev_date then begin
        fprintf oc "\\weekday{%s %s}\n"
          (Hashtbl.find days ss.s_date) (date_day ss.s_date);
        prev_date := ss.s_date
      end;
      output_session title room oc ss (* ~page ~room oc ss *)
    end;
  if title = night_track then fprintf oc "\\outputmap{}\n";
  fprintf oc "\\newpage\n\n"

let extract_night_events s =
  SMap.fold
    (fun track se l -> List.filter se ~f:(fun s -> s.s_time > night) @ l)
      s []

let lookup_title ~track ~date pages =
  match List.filter pages ~f:(fun (t,d,_) -> t = track && d = date) with
    [_,_,s] -> s
  | _ -> failwith "lookup_title"


let group_pages_by_date pages =
  let date_ps = group (List.map pages ~f:(fun p -> (p.tr_date, p))) in
  let res0 = List.map date_ps ~f:begin fun (date, ps) ->
                     let day = try Hashtbl.find days date with Not_found -> "Unknown" in
                     (day, date, ps)
                   end
  in
  let res1 = sort_by res0 ~prj:snd3 in
  let _ = List.iter res1 ~f:begin fun (day, date, _) ->
    Printf.eprintf "%s DAY: %s (%s) \n" "%%" date day
  end
  in
  res1

let output_overview ~days day_pages ~details oc =
  fprintf oc "\\header{%s}{%s}{%s -- %s}{Overview}\n\n"
    details.e_acronym details.e_location details.e_start details.e_end;
  List.iter day_pages ~f:begin fun (day, date, ps) ->
    fprintf oc "\\weekday{%s %s}\n" day (date_day date);
    List.iter ps ~f:begin fun p ->
      let name  = p.tr_name in
      let date  = p.tr_date in
      let title = p.tr_title in
      fprintf oc "\\overview{%s}{%s%s}{\\pageref{%s-%s}}\n"
        name title (day_string p.tr_day) title (date_day date)
    end;
    fprintf oc "\\closeday\n\n";
  end;
  fprintf oc "\\newpage\n\n"

let output_all ~days pages ~details oc sorted =
  let night_events = extract_night_events sorted in
  let pages =
    if night_events = [] then pages else
     { tr_name  = night_track
     ; tr_date  =  ""
     ; tr_title = "Evening Events"
     ; tr_day   = None
     } :: pages
  in
  let day_pages = group_pages_by_date pages in
  output_overview ~days day_pages ~details oc;
  (* HIDE output_page ~track:night_track ~title:"Evening Events" oc night_events; *)
  (* let _ = failwith ("PAGES = " ^ string_of_int (List.length pages)) in *)
  List.iter day_pages ~f:begin fun (day, date, ps) ->
    List.iter ps ~f:begin fun p ->
      output_page p oc (get_sessions p.tr_name sorted)
    end
  end

(* Full processing *)

let process ?out xml =
  let schedule = parse_file xml in
  let details = parse_event_details schedule in
  let subevs = extract_subevents schedule in
  (* let _      = failwith ("SUBEVS:" ^ string_of_int (List.length subevs)) in *)
  let sorted = sort_by_track subevs in
  let sorted_sessions = SMap.map (List.map ~f:parse_subevent) sorted in
  let oc = match out with None -> stdout | Some f -> open_out f in
  output_all ~days pages ~details oc sorted_sessions;
  if out <> None then close_out oc;;

let () =
  if Array.length Sys.argv >= 2
  && Filename.check_suffix Sys.argv.(1) ".xml" then
    (process Sys.argv.(1); exit 0)
  else
    (prerr_endline "Usage: schedule_parser schedule.xml > schedule.tex"; exit 1)
;;

(* Code after this line is not executed when compiled *)

(*
process "acmdlxml.xml" ~out:"schedule.tex";;

let schedule = parse_file "acmdlxml.xml"
let subevs = extract_subevents schedule
let sorted = sort_by_track subevs
let keys = List.map fst (SMap.bindings sorted)
let sorted_sessions = SMap.map (List.map ~f:parse_subevent) sorted;;
output_page ~track:"HOPE" stdout (get_sessions "HOPE" sorted_sessions);;
output_overview ~days ~pages stdout;;
output_all ~days ~pages stdout sorted_sessions;;
*)
