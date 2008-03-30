#!/usr/bin/ocamlrun /usr/bin/ocaml

#use "topfind"
#require "los log"

let dry_run = Argv.has "-n"

type address = {
  user: string option;
  host: string option;
  path: string
}

let whoami = Sh.line1 "whoami"
let localhost = Sh.line1 "hostname"

let parse_address addr =
  let re = Pcre.regexp "((^[^@]+)@([^:]+):)?(.+)$" in
  let a = Pcre.extract ~full_match:false ~rex:re addr in
  let addr =
    {user=(match a.(1) with "" -> None | s -> Some s);
     host=(match a.(2) with "" -> None | s -> Some s);
     path=a.(3)} in
  (* Special case for current user and local address *)
  match addr with
    {user=user; host=host; path=path} when user = whoami && host = localhost ->
      {user=None; host=None; path=path}
  | addr -> addr

let address_to_string =
  function
      {user=None; host=None; path=path} ->
	path
    | {user=user; host=host; path=path} ->
	Option.set "root" user
	^ "@" ^ Option.set "localhost" host
	^ ":" ^ path

(** If REMOTE is set the archive command is executed on a remote
    system
*)

let option_put a = function None -> Some a | x -> x

let rec archive ?(opts=[]) ?(ssh=[]) (srcaddr : string) (destaddr : string) =
  let src = parse_address srcaddr
  and dest = parse_address destaddr in
  match dest with
    {user=None; host=None; path=path} -> begin
      (* Destination is local, perform the backup *)
      let cmd = String.concat " " (ssh @ ["/home/dsf/bin/archive"] @ opts @ [address_to_string src; address_to_string dest]) in
      Log.put ~v:0 ("-> " ^ cmd);
      let strm = 
	if dry_run
	then Stream.of_list [Sh.Exit 0]
	else Sh.to_stream cmd in
      let error_output = Buffer.create 256 in
      try
	Stream.iter
	  (function
	       Sh.Stdout s ->
		 if Log.get_verbosity () > 0
		 then Log.put s
	     | Sh.Stderr s ->
		 if Log.get_verbosity () > 0
		 then Log.put s
		 else Buffer.add_string error_output s
	     | Sh.Exit 0 -> ()
	     | Sh.Exit n -> failwith ("Exit " ^ string_of_int n)
	     | Sh.Stopped n -> failwith ("Stopped " ^ string_of_int n)
	     | Sh.Signaled n -> failwith ("Signaled " ^ string_of_int n)
	     | Sh.Open -> failwith "Open")
	  strm;
	flush stderr; flush stdout;
	Log.put ~v:0 ("ok")
      with
	exn ->
	  Log.put ~v:0 ("failed:");
	  Log.indent ~v:1 ~prefix:"  > " (fun () -> Log.put ~v:0 (Buffer.contents error_output));
    end
  | {user=user; host=host; path=destpath} ->
      (* Destination is remote, execute the archive command remotely *)
      Log.put ~v:0 ("-> archive " ^ (address_to_string src) ^ " -> " ^ (address_to_string dest));
      let src = {src with
		   host=Some (Option.set "server" src.host);
		   user=Some (Option.set "root" src.user)}
      and dest = {user=None; host=None; path=destpath} in
      let ssh = ["ssh"; Option.set "root" user ^ "@" ^ Option.set "localhost" host] in
      Log.indent (fun () -> archive ~opts:opts ~ssh:ssh (address_to_string src) (address_to_string dest))

let elapsed f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  Log.put ~v:0 ("  elapsed: " ^ string_of_float (t1 -. t0));
  result
  
let base = "/home/backups/"

let cacheopts = ["--exclude"; "'/.mozilla/**/Cache/'"; "--exclude"; "'/.kde/share/cache/'"]
let dsfopts = cacheopts @ ["--exclude"; "'/tmp/'";
			   "--exclude"; "'/cvs/'";
			   "--exclude"; "'/tla/'";
			   "--exclude"; "'/images/'";
			   "--exclude"; "'/{archives}'"]

let specs =
  [
(*
    "root@server:/home/dsf/images",	"root@dsf:/home/backups/images",	[];
    "root@server:/home/dsf/cvsroot",	"root@dsf:/home/backups/dsfcvs",	[];
    "root@server:/home/dsf/{archives}",	"root@dsf:/home/backups/tla",		[];
    "root@server:/var/lib/geneweb",	"root@dsf:/home/backups/geneweb",	[];
    "root@server:/var/mail",		"root@dsf:/home/backups/dsfmail",	[];
    "root@server:/var/www",		"root@dsf:/home/backups/www",		[];
    "root@server:/home/audio",		"root@dsf:/home/backups/audio",		[];
    "root@dsf:/mnt/sda2/cdroms",		"root@server:/home/backups/cdroms",	[];
    "root@dsf:/home/dsf",		"root@server:/home/backups/dsf",	dsfopts;
*)
    "root@lydia:/mnt/hda2/root",	"root@server:/home/backups/ldt1",	cacheopts;
    "root@lydia:/mnt/hdb3/ldt",		"root@server:/home/backups/ldt2",	cacheopts;
    "root@lydia:/root",			"root@server:/home/backups/ldt3",	cacheopts;
    (*"root@dsf:/mnt/los-hdb3/ldt",	"/home/backups/ldt4",			[];*)
    (*"root@dsf:/home/backups/ldt/ldt-20030601",	"/home/backups/ldt5",		[];*)
    (*"root@lydia:/var/mail",		"/home/backups/ldtmail",		[]*)]
   
let _ =
  try
    if Unix.geteuid () != 0
    then failwith "You must run this program as root";
    (* Make a list of all user@host addresses *)
    let addresses =
      List2.uniq compare
	(Option.filter
	   (List.map
	      (function 
		   {user=Some user; host=Some host} -> Some (user ^ "@" ^ host)
		 | _ -> None)
	      (List.flatten
		 (List.map
		    (fun (src, dest, _) ->
		       [parse_address src; parse_address dest])
		    specs)))) in
    (* Test our connection to each address *)
    List.iter
      (fun address ->
	 if Log.sh ~verr:0 ~vout:0 ("ssh -o \"BatchMode yes\" -p 22 " ^ address ^ " pwd >/dev/null") != 0
	 then failwith ("Can't reach " ^ address))
      addresses;
    (* Perform the archiving *)
    List.iter
      (fun (source, dest, opts) ->
	 let opts = 
	   (if Argv.has "-v" then ["-v"] else [])
	   @ (if Argv.has "-P" then ["-P"] else [])
	   @ opts in
	 archive ~opts:opts source dest)
      specs;
    Log.put ~v:0 "done.";
    exit 0
  with
    Failure msg ->
      Log.put ~v:0 ("ERROR: " ^ msg);
      exit 1
