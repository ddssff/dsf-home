(************** CHANGELOG FILE ***************)

let sigre = Pcre.regexp "[ \t-]*([^ \t-].*)  ([^\n]*)"

type changelog_entry = {
  name: string;
  version: string;
  distribs: string list;
  urgency: string;
  text: string;
  uploader: string;
  date: string}

let to_string entry = 
  Printf.sprintf "%s (%s) %s; urgency=%s\n\n%s\n -- %s  %s\n" 
    entry.name entry.version (String.concat "," entry.distribs) entry.urgency
    entry.text entry.uploader entry.date

let list_split_after pred lst =
  List.rev
    (List.map List.rev
       (List.fold_left
	  (fun result item ->
	     match result with
	       [] -> [[item]]
	     | hd :: tl ->
		 if pred item
		 then [] :: (item :: hd) :: tl
		 else (item :: hd) :: tl)
	  [] lst))

let newline = Pcre.regexp "\n"
let signed = Pcre.regexp  "^ --"
let headre = Pcre.regexp "^([^ ]*) \\(([^\\)]*)\\) ([^;]*); urgency=(.*)$"
let tailre = Pcre.regexp "[ \t-]*([^ \t-].*)  ([^\n]*)"
let comma = Pcre.regexp ","
let white = Pcre.regexp "^[ \t]*$"

let rec strip_leading_blank_lines =
  function
      line :: etc when Pcre.pmatch ~rex:white line -> strip_leading_blank_lines etc
    | lines -> lines

let parse s =
  let lines = Pcre.split ~rex:newline s in
  let entries = list_split_after (Pcre.pmatch ~rex:signed) lines in
  let entries = List.map strip_leading_blank_lines entries in
  let entries = List.map (function
			      "" :: lst -> lst
			    | lst -> lst) entries in
  Option.filter
    (List.map
       (fun lines ->
	  try
	    let head = Pcre.extract ~full_match:false ~rex:headre (List.hd lines)
	    and tail = Pcre.extract ~full_match:false ~rex:tailre (List.hd (List.rev lines))
	    and body = strip_leading_blank_lines (List.rev (List.tl (List.rev (List.tl lines)))) in
	    match  head, tail with
	      [|name; version; distribs; urgency|], [|uploader; date|] ->
		Some {name=name;
		      version=version;
		      distribs=Pcre.split ~rex:comma distribs;
		      urgency=urgency;
		      text=String.concat "\n" body;
		      uploader=uploader;
		      date=date}
	    | _ -> None
	  with
	    _ -> None)
       entries)
