(* Standalone copy of Debian.my_version_compare *)

let verbose = 0

let vprerr_endline n msg =
  if verbose >= n then prerr_endline msg

module String3 = struct

  let iteri f s =
    let i = ref 0 in
    String.iter (fun c -> f !i c; i := !i + 1) s

  let mapi f s =
    let s' = (String.create (String.length s)) in
    iteri (fun i c -> s'.[i] <- f i s.[i]) s;
    s'

  let map f s = mapi (fun i c -> f c) s

  let init n f =
    let s = String.create n in
    for i = 0 to (n-1) do s.[i] <- f i done;
    s

  (** Return length of common suffix *)
  let suffix_length s1 s2 =
    let rec loop l n1 n2 =
      if n1 < 0 || n2 < 0
      then l
      else if s1.[n1] <> s2.[n2]
      then l
      else loop (l+1) (n1-1) (n2-1) in
    loop 0 (String.length s1 - 1) (String.length s2 - 1)

  (** [has_suffix suf str] - Does {i str} end with the suffix {i suf}? *)
  let has_suffix suf str =
    suffix_length suf str = String.length suf

  (** [drop_suffix suf s] - Drop suffix suf it it is at end of s,
      otherwise raise Not_found *)
  let drop_suffix suf str =
    if has_suffix suf str
    then String.sub str 0 ((String.length str) - (String.length suf))
    else raise Not_found

  (** [has_prefix s1 s2] - Return length of common prefix *)
  let prefix_length s1 s2 =
    let rec loop n =
      try
	if s1.[n] = s2.[n]
	then loop (n+1)
	else n
      with
	_ -> n in
    loop 0

  (** [has_prefix pre str] - Return true if str starts with pre *)
  let has_prefix pre str =
    prefix_length pre str = String.length pre

  (** drop "abcde" 3 -> "de" *)
  let drop s n = String.sub s n ((String.length s) - n)

  (** [drop_prefix pre str] - Drop the prefix pre from the string str, or
      raise Not_found if it doesn't have it. *)
  let drop_prefix pre str =
    if has_prefix pre str
    then drop str (String.length pre)
    else raise Not_found

end

(* From "The Debian Packaging Manual":

   The upstream-version and debian-revision parts are compared by dpkg
   using the same algorithm:

   The strings are compared from left to right.

   First the initial part of each string consisting entirely of
   non-digit characters is determined. These two parts (one of which
   may be empty) are compared lexically. If a difference is found it
   is returned. The lexical comparison is a comparison of ASCII values
   modified so that all the letters sort earlier than all the
   non-letters.

   Then the initial part of the remainder of each string which
   consists entirely of digit characters is determined. The numerical
   values of these two parts are compared, and any difference found is
   returned as the result of the comparison. For these purposes an
   empty string (which can only occur at the end of one or both
   version strings being compared) counts as zero.

   These two steps are repeated (chopping initial non-digit strings
   and initial digit strings off from the start) until a difference is
   found or both strings are exhausted. *)

exception Negative
exception Positive
exception Bad_version_number

(* "The lexical comparison is a comparison of ASCII values modified so
   that all the letters sort earlier than all the non-letters." *)

let ascii_modify =
  let small_a = min (Char.code 'a') (Char.code 'A')
  and small_z = min (Char.code 'z') (Char.code 'Z')
  and large_a = max (Char.code 'a') (Char.code 'A')
  and large_z = max (Char.code 'z') (Char.code 'Z') in
  let f n =
    if n < small_a then char_of_int (n + 52)
    else if n <= small_z then char_of_int (n - small_a)
    else if n < large_a then char_of_int (n + 26)
    else if n <= large_z then char_of_int (n - large_a + 26)
    else char_of_int (n)
  in
  let table = String3.init 256 f in
  fun c -> table.[int_of_char c]

(* Split a debian version into epoch/version/revision triple. *)

(* Split a debian version number into an epoch, version, revision triple.
   If present, the epoch will end in ":" and the revision will start with
   "-", otherwise they will be empty strings. *)

let evr_split =
  (* [epoch:]version[-revision] *)
  let epoch = "([0-9]+:)"
  and ver = "(([^-:]*)|(([^:]*)(-[^-]*)))" in
  let evr_re = Pcre.regexp ("^[ \t]*" ^ epoch ^ "?" ^ ver ^ "[ \t]*$") in

  fun v ->
    try
      match Pcre.extract ~rex:evr_re v with
	[|_; e; _; v1; _; v2; r|] -> (e, v1^v2, r)
      | _ -> raise Bad_version_number
    with _ -> raise Bad_version_number

(* This is calls evr_split but then strips off the epoch and converts
   it to an integer, and strips the leading "-" from the revision.  I'm
   not sure which is more useful.  Probably this one, even if I did call
   it "old". *)

let old_evr_split s =

  let (e,v,r) = evr_split s in
  let e = if e = "" then 0 else int_of_string (String3.drop_suffix ":" e)
  and r = if r = "" then "" else String3.drop_prefix "-" r
  in (e,v,r)

(* Split a string into numeric and non-numeric parts.  Make sure the
   resulting list always begins with non-numeric (Str.Text) segments,
   even if it is empty. *)

let version_split =
  let nre = Pcre.regexp "[0-9]+" in
  fun v -> 
    let l = Pcre.full_split ~rex:nre v in
    match l with
      Pcre.Delim _ :: _ -> Pcre.Text "" :: l
    | _ -> l

(* Quickly compare two unsigned big integers *)

let big_int_compare a b =

  let strip_zeros =
    let rex = Pcre.regexp "^0*(([1-9][0-9]*)|0)$" in
    fun s -> (Pcre.extract ~full_match:false ~rex:rex s).(0) in

  let a = strip_zeros a
  and b = strip_zeros b in
  match String.length a - String.length b with
    0 -> String.compare a b
  | n -> n

let compare =

  (* Given two split up version numbers, return a list of pairs. *)
  let rec zip l1 l2 =
    match l1, l2 with
      [], [] -> []
    | hd1::tl1, hd2::tl2 -> (hd1, hd2) :: (zip tl1 tl2)
    | [], (Pcre.Text hd2)::tl2 -> (Pcre.Text "", Pcre.Text hd2) :: (zip [] tl2)
    | [], (Pcre.Delim hd2)::tl2 -> (Pcre.Delim "0", Pcre.Delim hd2) :: (zip [] tl2)
    | (Pcre.Text hd1)::tl1, [] -> (Pcre.Text hd1, Pcre.Text "") :: (zip tl1 [])
    | (Pcre.Delim hd1)::tl1, [] -> (Pcre.Delim hd1, Pcre.Delim "0") :: (zip tl1 [])
    | Pcre.Group (_, _)::_, _ -> failwith "Unexpected"
    | Pcre.NoGroup::_, _ -> failwith "Unexpected"
    | _, Pcre.Group (_, _)::_ -> failwith "Unexpected"
    | _, Pcre.NoGroup::_ -> failwith "Unexpected"
  in

  let (upstream_version_compare : string -> string -> int) =
    let nul = String.make 1 (char_of_int 0) in
    fun v1 v2 ->
      (*prerr_endline ("upstream compare " ^ v1 ^ " " ^ v2);*)
      try
	List.iter
	  (function
	       (Pcre.Text s1), (Pcre.Text s2) ->		(* Text compare *)
		 (*prerr_endline ("s1: '" ^ s1 ^ "'\ns2: '" ^ s2 ^ "'");*)
		 let s1' = String3.map ascii_modify s1
		 and s2' = String3.map ascii_modify s2 in
		 (*prerr_endline ("s1': '" ^ s1' ^ "'\ns2': '" ^ s2' ^ "'");*)
		 (*prerr_endline ("  s1' < s2' -> " ^ (string_of_bool (s1' < s2')));*)
		 if s1' < s2' then raise Negative else
		   if s1' > s2' then raise Positive
	     | (Pcre.Delim s1), (Pcre.Delim s2) ->	(* Number compare *)
		 (*prerr_endline ("s1: '" ^ s1 ^ "'\ns2: '" ^ s2 ^ "'");*)
		 let c = big_int_compare s1 s2 in
		 (*prerr_endline ("big_int_compare " ^ s1 ^ " " ^ s2 ^ " -> " ^ (string_of_int c));*)
		 if c < 0 then raise Negative
		 else if c > 0 then raise Positive
	     | _, _ -> raise Bad_version_number)	(* Can't happen *)
	  (zip (version_split v1) (version_split v2));
	0
      with
	Negative ->
	  (*prerr_endline "negative";*)
	  (-1)
      | Positive ->
	  (*prerr_endline "positive";*)
	  1
  in

  fun v1 v2 ->
    let (e1,v1,r1) = old_evr_split v1
    and (e2,v2,r2) = old_evr_split v2 in
    vprerr_endline 2 (" e1='" ^ string_of_int e1 ^ "', v1='" ^ v1 ^ "', r1='" ^ r1 ^ "'");
    vprerr_endline 2 (" e2='" ^ string_of_int e2 ^ "', v2='" ^ v2 ^ "', r2='" ^ r2 ^ "'");
    let result = 
      if e1 != e2
      then e1 - e2
      else let u = upstream_version_compare v1 v2 in
      if u != 0 then u else upstream_version_compare r1 r2 in
    (*prerr_endline ("Result: " ^ (string_of_int result));*)
    result

let compare_v v1 v2 =
  let result = compare v1 v2 in
  vprerr_endline 2 ("Debversion.compare '" ^ v1  ^ "' '" ^ v2 ^ "' -> " ^ string_of_int result);
  result    
