(* Takes a function f:a' -> b' option
 * and maps it to the list l:a' list
 * keeping only the Some (b) results
 *)
let mapfilter f l =
	let rec driver acc x = match x with
		| [] -> List.rev acc
		| h::t -> (match f h with
			| None -> driver acc t
			| Some b -> driver (b::acc) t
		)
	in driver [] l