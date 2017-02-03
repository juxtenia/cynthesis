open Vil
module E = Errormsg

(* straightening optimisation *)

(* generate binputs for blocks *)
let generateconnections (m:funmodule) = List.iter 
	(fun m1 -> List.iter 
		(fun c ->
			match blockfromintoption m c.connectto with
				| Some m2 -> m2.binputs <- (c :: m2.binputs)
				| None -> ()
		) m1.boutputs
	) m.vblocks

(* merge two blocks together *)
let mergeblocks (m1:vblock) (m2:vblock) = 
	m1.boutputs <- m2.boutputs;
	m1.bvarexports <- m2.bvarexports;
	m1.bdataFlowGraph <- mergeoperations m1.bdataFlowGraph m2.bdataFlowGraph m1.boutputs;
	(* update the id's of the connections *)
	List.iter (fun c -> c.connectfrom <- Some m1.bid) m1.boutputs;
	(* return *)
	m1

(* merge block with redundant control flow *)
let rec compactblocks (acc:vblock list) (mods:vblock list) =
	match mods with
	| [] -> acc
	| h::t -> (match h.boutputs with
		| [{connectfrom = Some cfrom; connectto = Some cto; requires = None}] ->
			let filt = (fun m -> m.bid = cto)
			in let (it,rem,ac1) = if List.exists filt acc
				then let (it1,ac2) = List.partition filt acc in (it1,t,ac2)
				else let (it1,rem1) = List.partition filt t in (it1,rem1,acc)
			in (match it with
				| [m] -> if (List.length m.binputs = 1)
					(* add merged module to head of list in case of further merges *)
					then compactblocks ac1 ((mergeblocks h m) :: rem)
					(* skip module *)
					else compactblocks (h :: acc) t
				| _ -> E.s (E.error "Incorrect module connections or ids: [%s]\n" (String.concat ", " (List.map string_of_vblock it)))
			)
		| _ -> compactblocks (h :: acc) t
	)

(* Unreachable Code Removal *)

(* removes unreachable block *)
let rec removeunreachableblocks (mods:vblock list) = 
	let (rm,kp) = List.partition (fun b -> Listutil.empty b.binputs) mods
	in match rm with
		| [] -> kp
		| _ -> List.iter (fun b -> 
				b.binputs <- List.filter (fun c ->
					match c.connectfrom with
						| Some i -> not (List.exists (fun b1 -> b1.bid = i) rm)
						| None -> true
				) b.binputs
			) kp;
			removeunreachableblocks kp


(* Live Variable Analysis *)

(* adds a variable to the inputs of a module and the 
 * outputs of predecessors, then checks predecessors
 * if they assign the variable stop, otherwise add to them
 *)
let rec addvariable (f:funmodule) (m:vblock) (v:vvarinfo) = 
	if variableinlist v m.bvars
	then () 
	else (
		m.bvars <- v :: m.bvars;
		List.iter (fun from -> 
			(if variableinlist v from.bvarexports 
			then () 
			else from.bvarexports <- v :: from.bvarexports);
			if List.exists (fun op -> match op.operation with
				| Result (v1,_,_,_) when v1.varname = v.varname -> true
				| _ -> false) from.bdataFlowGraph
			then ()
			else addvariable f from v 
		) (getblockpredecessors f m)
	)

(* generate all variables in all blocks *)
let generatedataflow (f:funmodule) = List.iter
	(fun m -> List.iter 
		(fun o -> match o.operation with
				| Variable v -> addvariable f m v
				| _ -> ()
		) m.bdataFlowGraph) f.vblocks

(* removes assignments to variables that are not used later *)
let pruneresults (f:funmodule) = List.iter
	(fun m -> m.bdataFlowGraph <- List.filter
		(fun o -> match o.operation with
			| Result (v,_,_,_) when not (List.exists 
				(fun s -> variableinlist v s.bvars) (getblocksucessors f m)) -> false
			| _ -> true
		) m.bdataFlowGraph) f.vblocks

(* remove unused variables (caused by merging blocks, the cil optimiser, or dead source code) *)
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
			) m.bdataFlowGraph
		) f.vblocks
	) f.vlocals;
	(* do safety check to ensure variables at entry point are
	 * inputs and not local variables. Cil should have fixed
	 * undefined local variables.
	 *)
	let entry = getentrypoint f 
	in List.iter (fun v -> 
			if variableinlist v f.vlocals
			then E.s (E.error "Variable \"%s\" is undefined at entry point\n" v.varname)
			else ()
		) entry.bvars 

(* Dead Code elimination and duplicate operation removal *)

(* build operation counts *)
let rec dooperationcounts (cs:vconnection list) (ops:voperation list) = 
	List.iter incchildren ops;
	List.iter (fun o -> match o.operation with
	| Result (_,_,_,_) 
	| ReturnValue _ -> incoperationcount o
	| _ -> ()) ops;
	List.iter (fun c -> match c.requires with
		| None -> ()
		| Some (o,_) -> List.iter incoperationcount (getlinkchildren o)
	) cs

(* remove unreferenced ops, update children, repeat *)
let rec pruneoperations (os:voperation list) =
	if List.exists (fun o -> o.ousecount = 0) os
	then pruneoperations (List.filter (fun o -> if o.ousecount = 0 then (decchildren o; false) else true) os)
	else os

(* removes duplicate operations *)
let rec compactoperations (c:vconnection list) (acc:voperation list) 
		(skip:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> (match skip with
		| [] -> acc
		| _ -> compactoperations c acc [] skip
	)
	| h::t -> if(childreninlist true h acc)
		then
			let (eqs,neqs) = 
				List.partition (fun o -> eq_operation_type h.operation o.operation) t
			in let replacements = List.map (fun o -> (o,Simple h)) eqs
			in  replaceoperations replacements acc;
				replaceoperations replacements neqs;
				replaceoperations replacements skip;
				replaceconditions replacements c;
				compactoperations c (h::acc) skip neqs
		else compactoperations c acc (h::skip) t

(* optimises away unecessary operations *)
let culloperations (f:funmodule) = 
	List.iter (fun m -> 
		dooperationcounts m.boutputs m.bdataFlowGraph) f.vblocks;
	List.iter (fun m -> 
		m.bdataFlowGraph <- pruneoperations m.bdataFlowGraph
	) f.vblocks;
	List.iter (fun m ->
		m.bdataFlowGraph <- compactoperations m.boutputs [] [] m.bdataFlowGraph
	) f.vblocks;;

(* optimising entry point *)
let optimisefunmodule (f:funmodule) = 
		(* Generate connections. Adds all connections in 
		 * bouputs to the appropriate module's binputs
		 *)
		generateconnections f;
		(* Straightening optimisation 
		 * merge basic blocks with basic control flow
		 *)
		f.vblocks <- compactblocks [] f.vblocks;
		(* Unreachable code removal 
		 * repeatedly remove all blocks with no input connections, 
		 * and erase the connections from them in other modules
		 *)
		f.vblocks <- removeunreachableblocks f.vblocks;
		(* Live variable analysis
		 * annotates blocks with live variables,
		 * removes unnecessary Result (_,_) labels and
		 * removes unused local variables completely 
		 *)
		variablecull f;
		(* Dead code elimination and duplicate operation removal 
		 * removes any operations which are not needed by a 
		 * result label, control flow condition or return value
		 * also checks to see if blocks contain duplicate operations,
		 * removing surplus ones.
		 *)
		culloperations f;