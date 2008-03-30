let _ =
  Db.set_default_db {Mysql.dbhost=Some "localhost";
		     Mysql.dbname=Some "refinery";
		     Mysql.dbport=Some 3307;
		     Mysql.dbuser=Some "read";
		     Mysql.dbpwd=Some "ir2206";}

(*let result = Mysql.exec dbd "describe pkg_src_extra";;*)

let bin_to_src bin_name =
  let pkg_system_col = {Db.table="pkg_group_to_pkg_bin"; Db.field="pkg_system"}
  and name_col = {Db.table="pkg_group_to_pkg_bin"; Db.field="name"}
  and bin_name_col = {Db.table="pkg_group_to_pkg_bin"; Db.field="bin_name"} in
  let query = Db.Select ([name_col], 
			 Db.And [Db.Value (pkg_system_col, Db.String "debian");
				 Db.Value (bin_name_col, Db.String bin_name)]) in
  prerr_endline (Db.query2sql query);
  match List2.uniq compare (Db.dquery_1s query) with
    [Db.String src_name] -> src_name
  | _ -> failwith "Invalid result"

let versions name =
  let pkg_system_col = {Db.table="pkg_src_extra"; Db.field="pkg_system"}
  and version_col = {Db.table="pkg_src_extra"; Db.field="version"}
  and name_col = {Db.table="pkg_src_extra"; Db.field="name"} in

  let query = Db.Select ([version_col],
			 Db.And [Db.Value (pkg_system_col, Db.String "debian");
				 Db.Value (name_col, Db.String name)]) in

  prerr_endline (Db.query2sql query);
  let versions = Db.dquery_1s query in

  List.rev
    (List.sort Debversion.compare_v
       (List.map (function Db.String s -> s | _ -> "") versions))

let latest name =
  match versions name with
    [] -> raise Not_found
  | a :: _ -> a

let changelog name version =
  let raw_changelog_col = {Db.table = "pkg_src_extra"; Db.field = "raw_changelog"}
  and name_col = {Db.table = "pkg_src_extra"; Db.field = "name"}
  and version_col = {Db.table = "pkg_src_extra"; Db.field = "version"}
  and pkg_system_col = {Db.table = "pkg_src_extra"; Db.field = "pkg_system"} in

  let query = Db.Select ([raw_changelog_col],
			 Db.And [Db.Value (name_col, Db.String name);
				 Db.Value (version_col, Db.String version);
				 Db.Value (pkg_system_col, Db.String "debian")]) in
  prerr_endline (Db.query2sql query);
  match Db.dquery_1 query with
    Db.String s | Db.Blob s -> Debchangelog.parse s
  | _ -> failwith "Unexpected error"

let _ =
  match Array.length Sys.argv with
    4 ->
      let ver = latest Sys.argv.(1) in
      print_endline ("Latest " ^ Sys.argv.(1) ^ ": " ^ ver);
      print_endline ("Min: " ^ Sys.argv.(2));
      print_endline ("Max: " ^ Sys.argv.(3));
      let log = changelog Sys.argv.(1) ver in
      let range =
	List.filter
	  (fun entry ->
	     Debversion.compare_v entry.Debchangelog.version Sys.argv.(2) >= 0 && 
	       Debversion.compare entry.Debchangelog.version Sys.argv.(3) <= 0)
	  log in
      List.iter print_endline (List.map Debchangelog.to_string range)
  | n ->
      prerr_endline (Printf.sprintf "usage: %s <package> <minversion> <maxversion>" Sys.argv.(0))

(*
  ignore (Mysql.exec dbd "use refinery");
  pkg_bin_extra - contains changelogs
  Mysql.list_dbs dbd ();
  
  let result = Mysql.exec "describe warehouse" in
*)
