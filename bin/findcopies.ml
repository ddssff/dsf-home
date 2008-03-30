(* ocamlopt -I +los unix.cmxa los.cmxa findcopies.ml -o findcopies *)

let map_fst f (a, b) = (f a, b)

let args = List.tl (Array.to_list Sys.argv)
let (quiet, args) = map_fst List.length (List.partition ((=) "-q") args)
let (verbose, args) = map_fst List.length (List.partition ((=) "-v") args)
let quiet = quiet - verbose

let files_per_dot = 1024
(* FIXME: dots should represent some number of bytes compared *)
let cmps_per_dot = 1048576L

(* Given one or more directories as arguments, print a list of files
   with identical content.  This is done by sorting by size and
   comparing the members of each size group.  Note that this is a very
   different task from linkcopies, which also takes the path into
   consideration. *)

let (minsize, args) =
  map_fst
    (function [] -> 5000L | x :: _ -> Int64.of_string x)
    (List.partition ((=) "--minsize") args)

(* Record possible duplicate directories *)
let pathtable = Hashtbl.create 1024
let rec countpath path1 path2 =
  let path1 = Filename.dirname path1
  and path2 = Filename.dirname path2 in
  if path1 <> "." && path2 <> "." && path1 <> "/" && path2 <> "/" && path1 <> path2 then begin
    Hashtbl.replace pathtable (path1, path2) ((try Hashtbl.find pathtable (path1, path2) with _ -> 0) + 1);
    countpath path1 path2
  end

let _ =
  let files = ref [] in
  let filecount = ref 0 in
  if quiet < 2 then prerr_endline "Scanning directories...";
  List.iter
    (fun dir ->
       Dir.find
	 (fun path stats ->
	    if compare minsize stats.Unix.LargeFile.st_size < 0 &&
	      stats.Unix.LargeFile.st_kind = Unix.S_REG
	    then begin
	      files := 
		(path,
		 stats.Unix.LargeFile.st_size,
		 stats.Unix.LargeFile.st_dev,
		 stats.Unix.LargeFile.st_ino) :: !files;
	      filecount := !filecount + 1;
	      if quiet < 2 && !filecount mod files_per_dot = 0
	      then begin output_string stderr "."; flush stderr end
	    end)
	 dir)
    args;
  if quiet < 2 then prerr_endline "Sorting files...";

  (* Group files that are the same size *)
  let precmp1 =
    fun (path1, size1, dev1, ino1) (path2, size2, dev2, ino2) ->
      compare size2 size1 in
  let groups = List2.group precmp1 (List.sort precmp1 !files) in

  (* Filter out groups of files that are too small *)
  let groups =
    List.filter (function
		     (_, size, _, _) :: _ -> size >= minsize
		   | _ -> true) groups in

  (* Group hard links and keep only the first *)
  let precmp2 =
    fun (path1, size1, dev1, ino1) (path2, size2, dev2, ino2) ->
      match dev1 - dev2 with 0 -> ino1 - ino2 | n -> n in
  let groups =
    List.map
      (fun group ->
	 let hardlinks = List2.group precmp2 group in
	 List.map (function (hd :: _) -> hd | [] -> failwith "List2.group") hardlinks)
      groups in

  if quiet < 2 then prerr_endline ("Number of groups: " ^ string_of_int (List.length groups));
  if quiet < 0 then
    List.iter 
      (function
	   [] -> failwith "List2.group"
	 | (((path, size, dev, ino) :: _) as group) ->
	     print_endline ("group of " ^ string_of_int (List.length group) ^ " files of size " ^ Int64.to_string size))
      groups;
  (* The number of comparisons we need to perform on each group is n take 2 / 2, or n * (n - 1) / 2 *)
  let cmpcount = ref 0
  and cmpbytes = ref 0L in
  List.iter
    (function
	 [] -> ()
       | ((_, size, _, _) :: _) as group ->
	   let len = List.length group in
	   let cmps = len * (len - 1) / 2 in
	   cmpcount := !cmpcount + cmps;
	   cmpbytes := Int64.add !cmpbytes (Int64.mul size (Int64.of_int cmps)))
    groups;
  if quiet < 2 then prerr_endline ("Number of comparisons: "
				   ^ string_of_int !cmpcount);
  if quiet < 2
  then prerr_endline ("Bytes of comparison: "
		      ^ Int64.to_string (Int64.div !cmpbytes cmps_per_dot) ^ "M");

  let dupsize = ref 0L in
  let cmpsize = ref 0L
  and cmpdots = ref 0L in

  (* Find the copies within each group *)
  let realcmp =
    fun (path1, size1, dev1, ino1) (path2, size2, dev2, ino2) ->
      let newcmpsize = Int64.add !cmpsize size1 in
      let olddots = Int64.div !cmpsize cmps_per_dot in
      let newdots = Int64.to_int (Int64.sub (Int64.div newcmpsize cmps_per_dot) olddots) in
      for i = 1 to newdots do begin output_string stderr "."; flush stderr end done;
      cmpsize := newcmpsize;
      let cmd = "cmp -s '" ^ path1 ^ "' '" ^ path2 ^ "'" in
      match Sys.command cmd with
	0 ->
	  if quiet < 1 then Printf.printf "Duplicate: '%s', '%s', %Ld bytes\n" path1 path2 size2;
	  dupsize := Int64.add !dupsize size1;
	  countpath path1 path2;
	  0
      | n -> n in

  List.iter
    (fun group ->
       let rec loop files =
	 match files with
	   [] -> ()
	 | [hd] -> ()
	 | (_, 0L, _, _) :: _ ->
	     Printf.printf "%d files of size zero:\n  " (List.length files);
	     List.iter (fun (path, _, _, _) -> output_string stdout ("  " ^ path ^ "\n")) files; 
	 | f1 :: tl ->
	     List.iter
	       (fun f2 ->
		  ignore (realcmp f1 f2))
	       tl;
	     loop tl in
       loop group)
    groups;

  if quiet < 2
  then Printf.printf "Total size of duplicates: %Ld\n" !dupsize;
  let counts = ref [] in
  Hashtbl.iter (fun paths count -> counts := (count, paths) :: !counts) pathtable;
  match !counts with
    [] -> ()
  | lst ->
      Printf.printf "Number of common files:\n";
      List.iter
	(fun (count, (path1, path2)) -> Printf.printf "  %d: %s, %s\n" count path1 path2) 
	(List.sort (fun (a, _) (b, _) -> b - a) !counts)
