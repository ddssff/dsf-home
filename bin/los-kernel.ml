#!/usr/bin/ocamlrun /usr/bin/ocaml

#use "topfind"
#require "unix pcre"

module My = struct

  let has_suffix suff str =
    let len1 = String.length suff
    and len2 = String.length str in
    try String.sub str (len2 - len1) len1 = suff
    with _ -> false

end

type tla_version = {
  archive: string;
  category: string;
  branch: string;
  version: string;
}

(* Develop a list of TLA branches *)

let packages =
  ["acx100";
   "adm8211";
   "alsa";
   "ati-driver";
   "atmelwlandriver";
   "bcm5700";
   "cloop";
   "dazuko";
   "drm";
   "hostap";
   "hsflinmodem";
   "intel536ep";
   "intel537ep";
   "ipw2100";
   "ipw2200";
   "ivtv";
   "linux-wlan-ng";
   "los-ralink2500usb";
   "los-ralinkpci";
   "ltmodem";
   "madwifi";
   "ndiswrapper";
   "nvidia-driver";
   "paragon-ntfs";
   "pctel";
   "pwc";
   "pwcx";
   "realtime";
   "slmdm";
   "unionfs";
   "sis18x";
   "los-kernel-source";
   "los-kernel-image";
   "los-kernel-suite"]

let module_categories = function
    ("los-kernel-suite" | "los-kernel-image") as name ->
					[name; name ^ "-smp"]
  | "pctel" as name ->			[name ^ "-modules"]
  | "los-kernel-source" ->		[]
  | name ->				[name ^ "-modules"; name ^ "-modules-smp"]

let source_categories = function
    ("los-kernel-suite" | "los-kernel-image"
    | "bcm5700" | "cloop" | "hostap") as name ->[]
  | "alsa" as name ->				[name ^ "-driver"]
  | "realtime" as name ->			[name ^ "-lsm"]
  | ("los-kernel-source" | "unionfs" | "pwc" | "pwcx") as name ->
						[name]
  | name ->					[name ^ "-source"]

let categories name = source_categories name @ module_categories name

let archives_of_category = function
    "paragon-ntfs-source"
  | "paragon-ntfs-modules"
  | "paragon-ntfs-modules-smp"
  | "sis18x-modules"
  | "sis18x-modules-smp"
  | "unionfs"
  | "unionfs-modules"
  | "unionfs-modules-smp" ->			["tos@linspire.com--2005"]
  | "pwc" | "pwcx" | "sis18x-source" ->		["david@linspire.com--2005"]
  | "los-kernel-source"
  | "los-kernel-image" ->			["tos@lindows.com--2004"; "kernel@linspire.com--2005"]
  | _ ->					["tos@lindows.com--2004"]

let branches_of_category = function
    (_, "alsa-modules")
  | (_, "alsa-modules-smp")
  | (_, "paragon-ntfs-source")
  | (_, "paragon-ntfs-modules")
  | (_, "paragon-ntfs-modules-smp") ->	["debian-los"; "build-marlin"]
  | (_, "alsa-driver")
  | (_, "realtime-lsm") ->		["debian"; "build-marlin"]
  | (_, "pwc")
  | (_, "pwcx")
  | (_, "unionfs")
  | (_, "unionfs-modules")
  | (_, "unionfs-modules-smp") ->	["linspire"; "build-marlin"]
  | ("tos@lindows.com--2004", "los-kernel-source") ->
					["los-debian"; "build-marlin"]
  | ("kernel@linspire.com--2005", "los-kernel-source")
  | ("kernel@linspire.com--2005", "los-kernel-image") ->
      (* starting with 2.6.13 *)
					["linspire"; "build-marlin"]
  | (_, "ipw2100-source")
  | (_, "ipw2200-source") ->		["los-changes"; "build-marlin"]
  | (_, "drm-source") ->		["los"; "build-marlin"]	(* Not linspire *)
  | (_, "sis18x-source")
  | (_, "sis18x-modules")
  | (_, "sis18x-modules-smp") ->	["linspire"; "build-marlin"]
  | _ ->				["los-debian"; "build-marlin"]

let versions_of_branch = function
    (_, "alsa-driver", _) ->				["1.0.9.999"]
  | (_, "drm-source", _) ->				["0.20050302"]
  | (_, "pctel-modules", _) ->				[]
  | (_, "realtime-lsm", _) ->				["0.1.1"]
  | (_, "unionfs", _) ->				["1.0.11"]
  | (_, "pwc", _) ->					["9.0.2"]
  | (_, "pwcx", _) ->					["9.0"]
  | (_, "sis18x-source", _) ->				["1.00.00"]
  | (_, "los-ralink2500usb-source", _) ->		["1"]
  | (_, "los-ralinkpci-source", _) ->			["1.1.0"]
  | ("tos@lindows.com--2004", "los-kernel-source", _) ->
							["2.6.10"; "2.6.11"; "2.6.12"]
  | ("kernel@linspire.com--2005", "los-kernel-source", _) ->
							["2.6.13"]
  | (_, name, _) when My.has_suffix "-source" name ->	["0"]
  | (_, "paragon-ntfs-modules", _)
  | (_, "paragon-ntfs-modules-smp", _) ->		["2.6.11"; "2.6.12"]
  | (_, "los-ralink2500usb-modules", "build-marlin") ->	["2.6.11"; "2.6.12"]
  | (_, "los-ralinkpci-modules", "build-marlin") ->	["2.6.11"; "2.6.12"]
  | (_, "sis18x-modules", _)
  | (_, "sis18x-modules-smp", _) ->			["2.6.11"; "2.6.12"]
  | ("kernel@linspire.com--2005", _, _) ->		["2.6.13"]
  | _ ->						["2.6.10"; "2.6.11"; "2.6.12"]

(****************************************)

type action =
    Nil
  | Checkout
  | Packages			(* Run dh_listpackages *)
  | Merge of string
  | Tag of string * string

type params = {
  packages: string list;
  action: action;
  dry_run: bool;
  quiet: bool;
  ask: bool;
}

let params =
  let params = {action=Nil; dry_run=false; quiet=false; packages=[]; ask=false} in
  let rec loop = function
      params, "--tag" :: oldk :: newk :: etc ->
	loop ({params with action=Tag (oldk, newk)}, etc)
    | params, "--checkout" :: etc ->
	loop ({params with action=Checkout}, etc)
    | params, "--merge" :: kernel :: etc ->
	loop ({params with action=Merge kernel}, etc)
    | params, "--packages" :: etc ->
	loop ({params with action=Packages}, etc)
    | params, "-n" :: etc ->
	loop ({params with dry_run=true}, etc)
    | params, "-q" :: etc ->
	loop ({params with quiet=true}, etc)
    | params, "-i" :: etc ->
	loop ({params with ask=true}, etc)
    | params, arg :: etc ->
	loop ({params with packages=(arg :: params.packages)}, etc)
    | params, [] ->
	params in
  loop (params, (List.tl (Array.to_list Sys.argv)))

let params =
  {params with
     packages=(match params.packages with
		 [] -> packages
	       | _ -> params.packages)}

let package_categories =
  List.flatten (List.map categories params.packages)

let package_archives =
  List.flatten
    (List.map
       (fun category ->
	  List.map
	    (fun archive -> archive, category)
	    (archives_of_category category))
       package_categories)

let package_branches =
  List.flatten
    (List.map
       (fun (archive, category) ->
	  List.map
	    (fun branch -> archive, category, branch)
	    (branches_of_category (archive, category)))
       package_archives)

let package_versions =
  List.flatten
    (List.map
       (fun (archive, category, branch) ->
	  List.map
	    (fun version -> {archive=archive; category=category; branch=branch; version=version})
	    (versions_of_branch (archive, category, branch)))
       package_branches)

(****************************************)
    
let versions versions (archive, category, branch) =
  List.flatten
    (List.map 
       (fun kernel_version ->
	  [{archive=archive; category=category; branch=branch; version=kernel_version};
	   {archive=archive^"-smp"; category=category; branch=branch; version=kernel_version}])
       versions)  

let do_command cmd = 
  if params.dry_run
  then begin if not params.quiet then prerr_endline cmd end
  else begin
    let ok =
      if params.ask then
	let rec loop () =
	  prerr_string ("Next command:\n  " ^ cmd ^ "\nContinue? [y/n] "); flush stderr;
	  let answer = input_line stdin in
	  if Pcre.pmatch ~pat:"^[yY]$" answer
	  then true
	  else if Pcre.pmatch ~pat:"^[nN]$" answer
	  then false
	  else loop () in
	loop ()
      else true in
    if not ok then failwith "Aborting";
    match Sys.command cmd with
      0 -> ()
    | n -> failwith (cmd ^ " -> " ^ string_of_int n)
  end

let string_of_version {archive=archive; category=category; branch=branch; version=version} =
  archive ^ "/" ^ category ^ "--" ^ branch ^ "--" ^ version

let dir_of_version v =
  (*Unix.getenv "HOME"*) "/home/dsf" ^ "/tla/" ^ string_of_version v

let tag_command v1 v2 =
  "tla tag --setup " ^ string_of_version v1 ^ " " ^ string_of_version v2

let merge_command v1 v2 =
  "cd " ^ dir_of_version v2 ^ " && tla star-merge -t " ^ string_of_version v1

let print_version version = 
  print_endline (string_of_version version)

let for_all_package_versions f =
  List.iter f package_versions

let for_all_categories f =
  List.iter f package_categories

(* PREDICATES *)

let has_suffix suff str =
  let len1 = String.length suff
  and len2 = String.length str in
  len1 <= len2 && String.sub str (len2 - len1) len1 = suff

let is_source_version {category=category} =
  not (has_suffix "-modules" category) &&
  not (has_suffix "-modules-smp" category) &&
  not (has_suffix "-image" category) &&
  not (has_suffix "-image-smp" category) &&
  not (has_suffix "-suite" category) &&
  not (has_suffix "-suite-smp" category)

let is_build_branch {branch=branch} =
  List.mem branch ["build-marlin"; "build-marlin-unstable"]

let is_kernel_version kernel {version=version} =
  kernel = version

(* ITERATOR *)

let for_all_versions f =
  List.iter f package_versions

(* HIGH LEVEL OPERATIONS *)

let quote s = "\"" ^ s ^ "\""

let update target =
  let dir = Unix.getenv "HOME" ^ "/tla" in
  let name = string_of_version target in
  let loc = dir ^ "/" ^ name in
  let cmd = "cd " ^ quote loc ^ " && tla update" in
  prerr_endline ("UPDATE: " ^ name);
  do_command cmd

let checkout ({archive=archive} as target) =
  let dir = Unix.getenv "HOME" ^ "/tla" in
  let name = string_of_version target in
  let loc = dir ^ "/" ^ name in
  if Sys.file_exists loc
  then update target
  else begin
    let cmd = "cd " ^ quote dir ^ " && tla get -A " ^ quote archive ^ " " ^ quote name ^ " " ^ quote name in
    prerr_endline ("CHECKOUT: " ^ name);
    do_command cmd
  end

let list_packages ({archive=archive} as target) =
  let name = string_of_version target in
  let dir = Unix.getenv "HOME" ^ "/tla/" ^ name in
  if not (Sys.file_exists dir) then checkout target;
  output_string stdout (name ^ ":	"); flush stdout;
  let cmd = "cd " ^ quote dir ^ " && dh_listpackages" in
  do_command cmd

let tag_for_new_kernel oldkernel newkernel =
  for_all_versions
    (fun ({version=version} as v) ->
       if not (is_source_version v) && not (is_build_branch v) && v.version = oldkernel then begin
	 let newv = {v with version=newkernel} in
	 let buildv = {newv with branch="build-marlin"} in
	 let cmd1 = tag_command v newv
	 and cmd2 = tag_command newv buildv in
	 begin try checkout newv with _ -> do_command cmd1 end;
	 begin try checkout buildv with _ -> do_command cmd2 end;
	 checkout newv;
	 checkout buildv
       end)

let merge_into_build kernel =
  for_all_versions
    (fun ({version=version} as v) ->
       if (*not (is_source_version v) &&*) not (is_build_branch v) && v.version = kernel then begin
	 let newv = {v with branch="build-marlin"} in
	 let cmd1 = merge_command v newv
	 and cmd2 = "cd " ^ dir_of_version newv ^ " && tla commit -L merge" in
	 do_command cmd1;
	 do_command cmd2;
       end)

let _ =
  match params.action with
    Nil -> ()
  | Checkout  -> for_all_package_versions checkout
  | Packages  -> for_all_package_versions list_packages
  | Merge kernel -> merge_into_build kernel
  | Tag (oldk, newk) -> tag_for_new_kernel oldk newk
