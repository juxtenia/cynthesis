(** quick function to check if a list is empty *)
let empty l = match l with | [] -> true | _ -> false

(** Takes a function f:a' -> b' option
 *  and maps it to the list l:a' list
 *  keeping only the Some (_) results
 *  and extracting the items
 *)
let mapfilter f l =
	let rec driver acc x = match x with
		| [] -> List.rev acc
		| h::t -> (match f h with
			| None -> driver acc t
			| Some b -> driver (b::acc) t
		)
	in driver [] l

(** takes a function a' -> b' list
 *  and maps it to the list l:a' list
 *  collecting the results together
 *)
let mapflatten f l =
	List.flatten (List.map f l)

(** Gives the index of item i in list l	
 *)
 let indexof v l = 
 	let rec driver i l1 = match l1 with 
 		| [] -> raise Not_found
 		| h :: t -> if h = v then i else driver (i+1) t
 	in driver 0 l
