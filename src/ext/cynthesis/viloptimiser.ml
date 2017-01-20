open Vil

(* gives the result of running all replacements in reps on the operation op *)
let replaceone (reps :(voperation*voperation) list) (op: voperation) = 
	try snd (List.find (fun (f,_) -> f.oid = op.oid) reps)
	with | Not_found -> op

(* replace the targeted operations in condition requirements *)
let replaceconditions (reps :(voperation*voperation) list) (cs:vconnection list) = List.iter
	(fun c -> let req = 
		match c.requires with
			| None -> None
			| Some (o,b) -> Some (replaceone reps o,b)
		in c.requires <- req
	) cs

(* replace operations in the sub trees of operations in the list *)
let replaceoperations (reps :(voperation*voperation) list) (ops :voperation list) = List.iter 
	(fun o -> let op = 
		match o.operation with
			| Result (v,o1) -> Result(v,replaceone reps o1)
			| Unary (u,o1,t) -> Unary(u,replaceone reps o1,t)
			| Binary (b,o1,o2,t) -> Binary(b,replaceone reps o1, replaceone reps o2,t)
			| _ -> o.operation
		in o.operation <- op
	) ops

(* merge two modules together *)
let mergemodules (m1:vmodule) (m2:vmodule) = 
	let ret = {
			mid = m1.mid;
			minputs = m1.minputs;
			moutputs = m2.moutputs;
			mvars = m1.mvars;
			mdataFlowGraph = [];
		}
	(* remove result tags if one exists in second module *)
	in let first_half = List.filter 
		(fun o -> match o.operation with
			| Result(i,_) when hasvariableresult i m2 -> false
			| _ -> true
		) m1.mdataFlowGraph
	(* things to replace, variable references and what they 
	 * were set to in the first module *)
	in let replacements = ref []
	(* remove variable accesses that were set in first module and add replacements *)
	in let second_half = List.filter
		(fun o -> match o.operation with
			| Variable i when List.exists 
				(fun o1 -> match o1.operation with
					| Result(i1,o2) when i1.varname = i.varname -> 
						replacements := (o,o2) :: !replacements;
						true
					| _ -> false
				) m1.mdataFlowGraph -> false
			| _ -> true
		) m2.mdataFlowGraph
	in  (* do replacements *)
		replaceoperations !replacements second_half;
		(* join the two together (rev_append is tail recursive) *)
		ret.mdataFlowGraph <- List.rev_append first_half second_half;
		(* update the id's of the connections *)
		List.iter (fun c -> c.connectfrom <- Some ret.mid) ret.moutputs;
		(* do the same replacements on the condiditons *)
		replaceconditions !replacements ret.moutputs;
		(* return *)
		ret;; 

(* merge modules with redundant control flow *)
let rec compactmodules (acc:vmodule list) (mods:vmodule list) =
	match mods with
	| [] -> acc
	| h::t -> (match h.moutputs with
		| [{connectfrom = Some cfrom; connectto = Some cto; requires = None}] ->
			let filt = (fun m -> m.mid = cto)
			in let (it,rem,ac1) = if List.exists filt acc
				then let (it1,ac2) = List.partition filt acc in (it1,t,ac2)
				else let (it1,rem1) = List.partition filt t in (it1,rem1,acc)
			in (match it with
				| [m] -> if (List.length m.minputs = 1)
					(* add merged module to head of list in case of further merges *)
					then compactmodules ac1 ((mergemodules h m) :: rem)
					(* skip module *)
					else compactmodules (h :: acc) t
				| _ -> E.s (E.error "Incorrect module connections or ids: [%s]" (String.concat ", " (List.map string_of_vmodule it)))
			)
		| _ -> compactmodules (h :: acc) t
	)

(* generate minputs for modules *)
let generateconnections (m:funmodule) = List.iter 
	(fun m1 -> List.iter 
		(fun c ->
			match modulefromintoption m c.connectto with
				| Some m2 -> m2.minputs <- (c :: m2.minputs)
				| None -> ()
		) m1.moutputs
	) m.vmodules;
	let count = ref 0
	in List.iter (fun m1 -> if List.length m1.minputs = 0 
		then (count := !count + 1; 
			m1.minputs <- [{connectfrom = None; connectto = Some m1.mid; requires = None}])
		else ()
	) m.vmodules;
	if not (!count = 1) 
		then E.s (E.error "%d <> 1 entry points to function %s" !count m.vdesc.varname)
	;;

let rec addvariable (f:funmodule) (m:vmodule) (v:vvarinfo) = 
	if variableinlist v m.mvars
	then () 
	else (
		m.mvars <- v :: m.mvars;
		List.iter (fun from -> 
			if List.exists (fun op -> match op.operation with
				| Result (v1,_) when v1.varname = v.varname -> true
				| _ -> false) from.mdataFlowGraph
			then ()
			else addvariable f from v 
		) (getmodulepredecessors f m)
	)

let generatedataflow (f:funmodule) = List.iter
	(fun m -> List.iter 
		(fun o -> match o.operation with
				| Variable v -> addvariable f m v
				| _ -> ()
		) m.mdataFlowGraph) f.vmodules

let pruneresults (f:funmodule) = List.iter
	(fun m -> m.mdataFlowGraph <- List.filter
		(fun o -> match o.operation with
			| Result (v,_) when not (List.exists 
				(fun s -> variableinlist v s.mvars) (getmodulesucessors f m)) -> false
			| _ -> true
		) m.mdataFlowGraph) f.vmodules

(* remove unused variables (caused by merging modules) *)
let variablecull (f:funmodule) = 
	(* label appropriate variables *)
	generatedataflow f;
	(* remove useless result operations *)
	pruneresults f;
	(* remove unused locals all together *)
	f.vlocals <- List.filter
	(fun v -> List.exists 
		(fun m -> List.exists
			(fun o -> match o.operation with
				| Variable v1 when v1.varname = v.varname -> true
				| _ -> false
			) m.mdataFlowGraph
		) f.vmodules
	) f.vlocals;
	(* do safety check to ensure variables at entry point *)
	let entry = getentrypoint f 
	in List.iter (fun v -> 
			if variableinlist v f.vlocals
			then E.s (E.error "Variable \"%s\" is undefined at entry point" v.varname)
			else ()
		) entry.mvars 

(* build operation counts *)
let rec dooperationcounts (cs:vconnection list) (ops:voperation list) = 
	List.iter incchildren ops;
	List.iter (fun o -> match o.operation with
	| Result (_,_) -> incoperationcount o
	| _ -> ()) ops;
	List.iter (fun c -> match c.requires with
		| None -> ()
		| Some (o,_) -> incoperationcount o
	) cs

(* generate operation counts for all modules *)
let generateoperationcounts (f:funmodule) = 
	List.iter (fun m -> dooperationcounts m.moutputs m.mdataFlowGraph) f.vmodules

(* remove unreferenced ops, update children, repeat *)
let rec pruneoperations (os:voperation list) =
	if List.exists (fun o -> o.ousecount = 0) os
	then pruneoperations (List.filter (fun o -> if o.ousecount = 0 then (decchildren o; false) else true) os)
	else os

(* removes duplicate operations *)
let rec compactoperations (c:vconnection list) (acc:voperation list) (skip:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> (match skip with
		| [] -> acc
		| _ -> compactoperations c acc [] skip
	)
	| h::t -> if(childreninlist true h acc)
		then
			let (eqs,neqs) = 
				List.partition (fun o -> eq_operation_type h.operation o.operation) t
			in let replacements = List.map (fun o -> (o,h)) eqs
			in  replaceoperations replacements acc;
				replaceoperations replacements neqs;
				replaceoperations replacements skip;
				replaceconditions replacements c;
				compactoperations c (h::acc) skip neqs
		else compactoperations c acc (h::skip) t

(* optimises away unecessary operations *)
let culloperations (f:funmodule) = List.iter
	(fun m -> 
		m.mdataFlowGraph <- pruneoperations m.mdataFlowGraph
	) f.vmodules;
	List.iter (fun m ->
		m.mdataFlowGraph <- compactoperations m.moutputs [] [] m.mdataFlowGraph
	) f.vmodules;;

let optimisefunmodule (f:funmodule) = 
		(* Generate connections for compacting to use *)
		generateconnections f;
		(* Compact modules to basic blocks *)
		f.vmodules <- compactmodules [] f.vmodules;
		(* Remove unused variables *)
		variablecull f;
		(* Set up usage pointers for operation graphs *)
		generateoperationcounts f;
		(* remove unused operations *)
		culloperations f;