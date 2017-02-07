open Vil


(* code for filling in the schedule *)

(* sets the asap schedule for o *)
let asap (o:voperation) = o.oschedule <- 
	{
		earliest = (List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.earliest) (getchildren o))) + (operationoffset o);
		latest = o.oschedule.latest; 
		set = o.oschedule.set;
	}

(* sets the alap schedule for o*)
let alap (latest:int) (ops:voperation list) (o:voperation) = o.oschedule <- 
	{
		earliest = o.oschedule.earliest; 
		latest = (let users = List.filter (fun o1 -> List.memq o (getchildren o1)) ops 
			in match users with
				| [] -> latest
				| h::t -> (List.fold_left min (h.oschedule.latest - (operationoffset h)) (List.map (fun o1 -> o1.oschedule.latest - (operationoffset o1)) t)) 
		);
		set = o.oschedule.set;
	}

(* builds asap schedule for ops (start with acc=[]) *)
let rec generateasap (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (ready,notready) = List.partition (fun o -> childreninlist true o acc) ops
		in List.iter asap ready;
			generateasap (List.rev_append ready acc) notready
(* builds alap schedule for ops (start with acc=[] and latest as the maximum asap value) *)
let rec generatealap (latest:int) (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (notready,ready) = List.partition (fun o -> List.exists (fun o1 -> List.memq o (getchildren o1)) ops) ops
		in List.iter (alap latest acc) ready;
			generatealap latest (List.rev_append ready acc) notready

(* generates an overall schedule for the module *)
let rec generateschedule (m:vblock) =
	List.iter (fun o -> o.oschedule <- (* TODO add non trivial scheduler *)
	{earliest=o.oschedule.earliest;latest=o.oschedule.latest;set=o.oschedule.earliest}) m.bdataFlowGraph

(* generates schedules for all blocks *)
let generatescheduleinfo (f:funmodule) = 
	List.iter (fun m -> 
		generateasap [] m.bdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.bdataFlowGraph) [] m.bdataFlowGraph;
		generateschedule m;
	) f.vblocks

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