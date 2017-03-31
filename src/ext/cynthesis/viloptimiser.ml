open Vil
open Vilannotator
module S = Vil.S
module E = Errormsg

(* Straightening *)

(** merge two blocks together *)
let mergeblocks (m1:vblock) (m2:vblock) = 
	m1.boutputs <- m2.boutputs;
	m1.bvarexports <- m2.bvarexports;
	m1.bdataFlowGraph <- mergeoperations m1.bdataFlowGraph m2.bdataFlowGraph m1.boutputs;
	(* update the id's of the connections *)
	List.iter (fun c -> c.connectfrom <- Some m1.bid) m1.boutputs;
	(* return *)
	m1

(** merge block with redundant control flow *)
let rec compactblocks (acc:vblock list) (mods:vblock list) =
	match mods with
	| [] -> acc
	| h::t -> (match h.boutputs with
		| [{connectfrom = Some cfrom; connectto = Some cto; requires = None}] when cfrom <> cto ->
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

(** removes unreachable block *)
let rec removeunreachableblocks (mods:vblock list) = 
	let rec driver acc todo =
		match 
			List.partition (fun b -> 
				List.exists (fun c -> 
					match c.connectfrom with
						| None -> true
						| Some i -> List.exists (fun b1 -> b1.bid = i) acc
					) 
				b.binputs) 
			todo
		with
			| ([],_) -> acc
			| (add,ntodo) -> driver (List.rev_append add acc) ntodo
	in let blocks = driver [] mods
	in  List.iter (fun b -> 
			b.binputs <- List.filter (fun c -> match c.connectfrom with 
				| Some i when not (List.exists (fun b1 -> b1.bid = i) blocks) -> false
				| _ -> true
			) b.binputs;
			b.boutputs <- List.filter (fun c -> match c.connectto with 
				| Some i when not (List.exists (fun b1 -> b1.bid = i) blocks) -> false
				| _ -> true
			) b.boutputs
		) blocks;
		blocks

(** removes assignments to variables that are not used later *)
let pruneresults (f:funmodule) = List.iter
	(fun m -> m.bdataFlowGraph <- List.filter
		(fun o -> match o.operation with
			| Result (v,_,_,_) when not (List.exists 
				(fun s -> variableinlist v s.bvars) (getblocksucessors f m)) -> false
			| _ -> true
		) m.bdataFlowGraph) f.vblocks

(** remove unused variables (caused by merging blocks, the cil optimiser, or dead source code) *)
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

(** remove unreferenced ops, update children, repeat *)
let rec pruneoperations (os:voperation list) =
	if List.exists (fun o -> o.ousecount = 0) os
	then pruneoperations (List.filter (fun o -> if o.ousecount = 0 then (decchildren o; false) else true) os)
	else os

let deduplicate (cs:vconnection list) (ops:voperation list) =
	let classes = childclassesset [] S.empty ops
	in let rec driver acc reps c = match c with
		| [] -> acc
		| []::c1 -> List.iter (replaceoperations reps) c1;
			replaceconditions reps cs;
			driver acc [] c1
		| (h::t) :: c1 -> let (eqs,neqs) = 
				List.partition (fun o -> eq_operation_type h.operation o.operation) t
			in let replacements = List.fold_left (fun r o -> (o,Simple h)::r) reps eqs
			in  driver (h::acc) replacements (neqs::c1)
	in driver [] [] classes

(** optimises away unecessary operations *)
let culloperations (f:funmodule) = 
	List.iter (fun m -> List.iter 
		(fun o -> o.ousecount <- 0) m.bdataFlowGraph) f.vblocks;
	List.iter (fun m -> 
		dooperationcounts m.boutputs m.bdataFlowGraph) f.vblocks;
	List.iter (fun m -> 
		m.bdataFlowGraph <- pruneoperations m.bdataFlowGraph
	) f.vblocks;
	List.iter (fun m ->
		m.bdataFlowGraph <- deduplicate m.boutputs m.bdataFlowGraph
	) f.vblocks
                              
let peepholeopts (inits:vlookupinfo list) (v:vvarinfo) (o:voperation): 
(* Some ((ids to remove, ops to add ),  (list of (replace this, with this)) or None *)
        ((int list * voperation list) * ((voperation * voperationlink) list)) option = 
	match o.operation with
		(* constant folding *)
		| _ when Vilanalyser.constchildren o -> 
			let op = makeoperation (Constant {value=Vilanalyser.evaluate inits o;ctype=gettype v o})
			in Some(([o.oid],[op]),[(o,Simple op)])
		| Unary (Cast,Simple o1,t) when eq_type (gettype v o1) t 
			-> Some(([o.oid],[]),[(o,Simple o1)])
		| _ -> None

let rec peephole (inits:vlookupinfo list) (v:vvarinfo) (b:vblock) = 
	match Listutil.mapfilter (peepholeopts inits v) b.bdataFlowGraph with
		| [] -> ()
		| x -> let (lists,repls) = List.split x
			in let (rmls,addls) = List.split lists
			in let rmids = List.flatten rmls
			in let additions = List.flatten addls
			in let reps = List.flatten repls
			in  b.bdataFlowGraph <- List.filter (fun o1 -> not (List.mem o1.oid rmids)) b.bdataFlowGraph;
				b.bdataFlowGraph <- List.rev_append additions b.bdataFlowGraph;
				replaceoperations reps b.bdataFlowGraph;
				replaceconditions reps b.boutputs;
				peephole inits v b

let isused (s:string) (f:funmodule) =
	List.exists (fun b -> List.exists (fun o -> match o.operation with
		| Lookup(s1,_,_) when s1 = s -> true
		| _ -> false
	) b.bdataFlowGraph) f.vblocks

let getused (f:funmodule) (gl:vlookupinfo list) = 
	List.filter (fun g -> isused g.lookupname f) gl

(** optimising entry point *)
let optimisefunmodule (f:funmodule) = 
		(* inter block optimisations *)

		(* Generate connections. Wipes existing connections 
		 * (except entry points), then adds all connections 
		 * in bouputs to the appropriate module's binputs
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

		(* intra block optimisations *)

		(* Dead code elimination and duplicate operation removal 
		 * removes any operations which are not needed by a 
		 * result label, control flow condition or return value
		 * also checks to see if blocks contain duplicate operations,
		 * removing surplus ones.
		 *)
		culloperations f;
		(* Various peephole optimisations*)
		List.iter (peephole f.vglobals f.vdesc) f.vblocks;
		(* Live variable analysis
		 * annotates blocks with live variables,
		 * removes unnecessary Result (_,_) labels and
		 * removes unused local variables completely 
		 *)
		variablecull f;
		(* run this again to catch the dead code the variable cull reveals *)
		culloperations f;
		(* remove unused globals *)
		f.vglobals <- getused f f.vglobals;

