open Vil

(* generate control flow information *)

(** generate binputs for blocks *)
let generateconnections (m:funmodule) = 
	(* clear non entry connections *)
	List.iter (fun m1 -> m1.binputs <- List.filter 
		(fun c -> match c.connectfrom with
			| None -> true
			| Some _ -> false
		) m1.binputs) m.vblocks;
	(* generates new connections *)
	List.iter (fun m1 -> List.iter 
		(fun c ->
			match blockfromintoption m c.connectto with
				| Some m2 -> m2.binputs <- (c :: m2.binputs)
				| None -> ()
		) m1.boutputs) m.vblocks

(* Live Variable Analysis *)

(* adds a variable to the inputs of a module and the 
 * outputs of predecessors, recurses on all predecessors
 * without a result node for the variable
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
			if hasvariableresult v from.bdataFlowGraph
			then ()
			else addvariable f from v 
		) (getblockpredecessors f m)
	)
 
(** generate all variables in all blocks *)
let generatedataflow (f:funmodule) = 
	List.iter (fun m -> m.bvars <- []; m.bvars <- []) f.vblocks;
	List.iter
	(fun m -> List.iter 
		(fun o -> match o.operation with
				| Variable v -> addvariable f m v
				| _ -> ()
		) m.bdataFlowGraph) f.vblocks

(* Identify unreferenced operations *)

(** build operation counts *)
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

let setprobabilities (cs:vconnection list) (fs:float list) = 
	List.iter2 (fun c f -> c.probability <- f) cs fs

let loopprobabilitiesfromcount (c:int) (b:bool) = 
	let (tl,nl) = match c with
		| 0 -> (0.,1.)
		| _ -> let cf = 1. /. (float_of_int c) in (1.-.cf,cf)
	in if b then (tl,nl) else (nl,tl)

let annotateloopprobabilities (f:funmodule) =
	List.iter (fun b -> match Vilanalyser.getloopcount f b with
		| None -> ()
		| Some(c,b1) -> let (tp,fp) = loopprobabilitiesfromcount c b1
			in setprobabilities b.boutputs [tp;fp]
	) f.vblocks