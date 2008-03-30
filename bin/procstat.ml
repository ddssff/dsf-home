module File = struct

  (* This needs to work on filesystems like /proc where st_size is invalid. *)
  let to_string path =
    let chan = open_in path in
    let rec loop lst =
      let buf = String.create 1024 in
      match input chan buf 0 1024 with
	1024 -> loop (buf :: lst)
      | 0 -> String.concat "" (List.rev lst)
      | n when n > 0 -> loop (String.sub buf 0 n :: lst)
      | _ ->
	  close_in chan;
	  raise (Failure "File.to_string") in
    let s = loop [] in
    close_in chan;
    s

end

(* Repeatedly examine the status of the processes and compute each
   one's contribution to the load average. *)

type process = {
  pid: int;
  cmdline: string;
  running_count: int;
  sleeping_count: int;
  stopped_count: int;
  zombie_count: int;
  dead_count: int;
}

let loadavg () = 
  let text = File.to_string "/proc/loadavg" in
  String.sub text 0 (String.length text - 1)

let cpufreq () =
  let text = File.to_string "/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_cur_freq" in
  let khz = float_of_string (String.sub text 0 (String.length text - 1)) in
  khz /. 1000000.

type params = {
  reps: int;
  time: float;
}

let params =
  let rec loop params argv =
    match argv with
	"--reps" :: value :: etc ->
	  loop {params with reps = int_of_string value} etc
      | "--time" :: value :: etc ->
	  loop {params with time = float_of_string value} etc
      | arg :: etc -> 
	  prerr_endline ("Unrecognized argument: " ^ arg);
	  loop params etc
      | [] -> params in
  loop {reps = 100; time = 1.0} (List.tl (Array.to_list Sys.argv))

let _ =

  let reps = params.reps in
  let wait = params.time /. (float_of_int params.reps) in

  let stat_re = Pcre.regexp "^([0-9]+) [^ ]+ (.)"
  and pid_re = Pcre.regexp "^[0-9]+$"
  and nul_re = Pcre.regexp "\000" in

  let table = Hashtbl.create 50 in

  for i = 1 to reps do
    let h = Unix.opendir "/proc" in
    let s = Stream.from (fun n ->
			   try Some (Unix.readdir h)
			   with _ -> Unix.closedir h; None) in
    Stream.iter
      (function
	   pid when (Pcre.pmatch ~rex:pid_re pid) -> begin
	     match 
	       (try
		  Some (File.to_string ("/proc/" ^ pid ^ "/stat"),
			File.to_string ("/proc/" ^ pid ^ "/cmdline"))
		with 
		  _ -> None) with
		 None -> ()
	       | Some (stat, cmdline) ->
		   let a = Pcre.extract ~rex:stat_re stat in
		   let pid = a.(1)
		   and state = a.(2) in
		   for i = 0 to String.length cmdline - 1 do
		     if cmdline.[i] = '\000' then cmdline.[i] <- ' ' done;
		   let pid = int_of_string pid in
		   let proc =
		     try Hashtbl.find table pid
		     with Not_found ->
		       {pid=pid;
			cmdline=cmdline;
			running_count = 0;
			sleeping_count = 0;
			zombie_count = 0;
			stopped_count = 0;
			dead_count= 0} in
		   let proc =
		     match state.[0] with
		       'R' -> {proc with running_count = proc.running_count + 1}
		     | 'S' -> {proc with sleeping_count = proc.sleeping_count + 1}
		     | 'Z' -> {proc with zombie_count = proc.zombie_count + 1}
		     | 'T' -> {proc with stopped_count = proc.stopped_count + 1}
		     | 'D' -> {proc with sleeping_count = proc.sleeping_count + 1}
		     | 'X' -> {proc with dead_count = proc.dead_count + 1}
		     | _ -> prerr_endline ("Unexpected state: " ^ state); proc in
		   Hashtbl.replace table pid proc
	   end
	 | _ -> ())
      s;
(*
    List.iter 
      (fun (pid, avg, name, state) ->
	 Printf.printf "%2d %5s %s\t%s\n" avg pid name state)
      (List.sort (fun (_, avg1, _, _) (_, avg2, _, _) -> avg1 - avg2) !lst);
    flush stdout;
*)
    ignore (Unix.select [] [] [] wait)
  done;
  let lst = ref [] in
  Hashtbl.iter
    (fun pid proc ->
       lst := (proc.running_count, proc) :: !lst)
    table;
  Printf.printf "loadavg: %s, cpufreq: %.3f GHz\n\n" (loadavg ()) (cpufreq ());
  let lst = List.sort (fun (count1, _) (count2, _) -> count2 - count1) !lst in
  List.iter
    (fun (_, proc) -> 
       if proc.running_count > 0 then
	 Printf.printf 
	   "%5.1f%% %5d %-s\n"
	   ((float_of_int proc.running_count) *. 100.0 /. (float_of_int params.reps))
	   proc.pid (String.sub proc.cmdline 0 (min (String.length proc.cmdline) 67)))
    lst

(*  (%dR/%dS/%dT/%dZ/%dX) *)
