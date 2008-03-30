#!/usr/bin/ocamlrun /usr/bin/ocaml

#use "topfind"
#require "detect netstring"

let rec tags ?(indent="* ") =
  function
      Nethtml.Data s -> print_endline (indent ^ "data: \"" ^ String.escaped s ^ "\"")
    | Nethtml.Element (tag, _, docs) -> print_endline (indent ^ tag); List.iter (tags ~indent:("*"^indent)) docs

let _ =
  let page = Sh.string "curl -s http://www.directron.com/motherboards-coming-soon.html" in
  let docs = Nethtml.parse_document (Lexing.from_string page) in

  let rows =
    match docs with
      Nethtml.Data _
      :: Nethtml.Element
  	("html", _,
  	 [Nethtml.Element ("head", _, _);
  	  Nethtml.Element
  	    ("body", _,
  	     Nethtml.Element
  	       ("table", _,
  		[Nethtml.Element
  		   ("tr", _,
  		    Nethtml.Element ("td", _, _)
  		    :: Nethtml.Element ("td", _, _)
  		    :: Nethtml.Element
		      ("td", _,
		       Nethtml.Element ("table", _, _)
		       :: Nethtml.Element ("table", _, _)
		       :: Nethtml.Element ("hr", _, _)
		       :: Nethtml.Element ("table", _, _)
		       :: Nethtml.Element ("br", _, _)
		       :: Nethtml.Element ("table", _, x)
		       :: _
		      )
  		    :: _)])
  	     :: _)])
      :: _ -> x
    | _ -> failwith "parse problem 1" in

  let cells =
    List.flatten
      (List.map
	 (function
	      Nethtml.Element ("tr", _, cells) -> cells
	    | _ -> failwith "parse problem 2")
	 rows) in


  let names =
    Option.filter
      (List.map
	 (function
	      Nethtml.Element
		("td", _, 
		 [Nethtml.Element
		    ("font", _,
		     Nethtml.Element
		       ("b", _,
			[Nethtml.Element
			   ("a", _,
			    [Nethtml.Data name])])
		     :: _)]) -> Some name
	    | _ -> None)
	 cells) in
