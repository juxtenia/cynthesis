open Vil
open Vilevaluator
module E = Errormsg

let verbose = ref false
let domoduleprint = ref false

type optimisationtype =
	| ConditionalExpansion
	| LoopFlatten
	| LookupExpand

type optimisation = {
	blockid: int;
	desc: optimisationtype;
	estimatedvalue: int;
	apply: funmodule -> funmodule;
}

(** turns optimisationtype into a simple string representation *)
let string_of_optimisationtype ot = match ot with
	| ConditionalExpansion -> "ConditionalExpansion"
	| LoopFlatten -> "LoopFlatten"
	| LookupExpand -> "LookupExpand"

(** turns optimisation into a simple string representation *)
let string_of_optimisation o = "(" ^ string_of_int o.blockid ^ "," ^ string_of_optimisationtype o.desc ^ ")"

(** true iff the given optimisation will clone loop body *)
let isloopwideningtransform (o:optimisation) = match o.desc with
	| LoopFlatten -> true
	| _ -> false

let getconditionalexpansions (f:funmodule) = Listutil.mapfilter (fun b ->
	match b.boutputs with
		| [{connectto=Some tt;requires=Some(olt,true);};{connectto=tf;requires=Some(olf,false);}] 
			when eq_operation_link olt olf && let bt = blockfromint f tt
			in List.length bt.binputs = 1 && 
			(match bt.boutputs with
				| [{connectto=t;requires=None}] when t = tf -> true
				| _ -> false
			)
		-> Some {
			blockid=b.bid;
			desc=ConditionalExpansion;
			estimatedvalue=moduletime (blockfromint f tt);
			apply=(fun (f:funmodule) -> 
				let m = blockfromint f b.bid
				in let mt = blockfromint f tt
				in let sw = List.hd (getswitches m)
				in  mt.bdataFlowGraph <- mergeparralleloperations f.vdesc sw mt.bdataFlowGraph [];
					f.vblocks <- (Viloptimiser.mergeblocks m mt) :: 
						(List.filter (fun b1 -> b1.bid <> b.bid && b1.bid <> tt) f.vblocks);
					f
			)
		}
		| [{connectto=tt;requires=Some(olt,true)};{connectto=Some tf;requires=Some(olf,false)}] 
			when eq_operation_link olt olf && let bf = blockfromint f tf
			in List.length bf.binputs = 1 && 
			(match bf.boutputs with
				| [{connectto=t;requires=None}] when t = tt -> true
				| _ -> false
			)
		-> Some {
			blockid = b.bid;
			desc = ConditionalExpansion;
			estimatedvalue = moduletime (blockfromint f tf);
			apply = (fun (f:funmodule) -> 
				let m = blockfromint f b.bid
				in let mf = blockfromint f tf
				in let sw = List.hd (getswitches m)
				in  mf.bdataFlowGraph <- mergeparralleloperations f.vdesc sw [] mf.bdataFlowGraph;
					f.vblocks <- (Viloptimiser.mergeblocks m mf) :: 
						(List.filter (fun b1 -> b1.bid <> b.bid && b1.bid <> tf) f.vblocks);
					f
			)
		}
		| _ -> None
) f.vblocks

let swap f t i = match i with
	| None -> None
	| Some j when j = f -> Some t
	| Some j -> Some j

let getloopflattens (f:funmodule) = Listutil.mapfilter (fun b -> 
	match Vilanalyser.loopdesc f b with
		| []
		| [None] 
		| _::_::_ -> None
		| [Some b1] -> (match Vilanalyser.basicloop f b [Some b1] with
			| None -> None
			| Some i -> Some {
				blockid = b.bid;
				desc = LoopFlatten;
				estimatedvalue = i;
				apply = (fun (f:funmodule) -> 
					let block = blockfromint f b.bid 
					in let (lb,eb) = match getblocksucessors f block with
						| [t;f] -> if b1 then (t,f) else (f,t)
						| _ -> E.s (E.error "Boutputs doesn't match expected form")
					in  block.boutputs <- 
						{connectfrom=Some block.bid;connectto=Some lb.bid;requires=None;probability=1.;}::[];
					let loopblockids = block.bid :: (getallsucessors f block.bid lb)
					in let loopblocks = List.map (blockfromint f) loopblockids
					in let remblocks = List.filter (fun b -> not (List.mem b.bid loopblockids)) f.vblocks
					in let rec driver bs j = if j >= i then bs 
						else 
							let (m,ds) = Vilcopy.duplicate (bs @ loopblocks) loopblocks
							in  List.iter (fun b -> 
									List.iter (fun c -> 
										c.connectto <- swap block.bid (block.bid + m) c.connectto
									) b.boutputs) bs;
								List.iter (fun b -> 
									List.iter (fun c -> 
										c.connectto <- swap (block.bid + m) block.bid c.connectto
									) b.boutputs) ds;
								driver (ds @ bs) (j+1)
					in  f.vblocks <- driver remblocks 0;
						List.iter (fun b -> 
							List.iter (fun c -> 
								c.connectto <- swap block.bid eb.bid c.connectto
							) b.boutputs) f.vblocks;
						f
				);
			}
		)
) f.vblocks

let getlookupaddition (f:funmodule) = let name = ref "" in
try [Listutil.findfilter 
	( fun b ->
		if List.exists (fun o -> match o.operation with
			| Lookup(s,_,_) when maxtime (getchildren o) + 1 < o.oschedule.set
			-> name := s; true
			| _ -> false
		) b.bdataFlowGraph
		then Some {
			blockid = b.bid;
			desc = LookupExpand;
			estimatedvalue = 1;
			apply = (fun (f:funmodule) -> 
				List.iter (fun g -> 
					if g.lookupname = !name 
					then g.parrallelcount <- g.parrallelcount + 1
					else ()
				) f.vglobals;
				f
			)
		}
		else None
	) f.vblocks]
with | Not_found -> []

(** Gives the possible optimisation steps for f *)
let getpossibleoptimisations (f:funmodule) = 
	List.flatten
	[ 
		getconditionalexpansions f;
		getloopflattens f;
		getlookupaddition f;
	]



(** Gets the priority of one optimisation over another *)
let getadvantage (f:funmodule) (o1:optimisation) (o2:optimisation) = 
	o1.estimatedvalue
	(* if o2 will clone loop body, of which o1 operates on, then probably to do o1 first *)
	+ if isloopwideningtransform o2 && Vilanalyser.inloopbody f o1.blockid o2.blockid then 1000 else 0

let compareoptimisations (f:funmodule) (o1:optimisation) (o2:optimisation) = 
	(getadvantage f o1 o2) - (getadvantage f o2 o1)

(** Orders optimisations sensibly. 
 *  Only the first beneficial optimisation is used, so 
 *  the order can significantly affect performance.
 *)
let orderoptimisations  (f:funmodule) (os:optimisation list) = 
	List.sort (compareoptimisations f) os

(** Applies the optimisation to f *)
let applyoptimisation (f:funmodule) (o) = o.apply f

let opcostweight = 10.
let timecostweight = 200.
let lookupcostweight = 0.01

(** Evaluates how good a module is *)
let evaluate (f:funmodule) = 
	Vilscheduler.generatescheduleinfo f;
	let opcost = float_of_int (totaloperationcost f)
	in let lookupcost = float_of_int (totallookuparea f)
	in let timecost = weightedtimecost f
	in let totalcost = 
		lookupcostweight *. lookupcost +. 
		opcostweight *. opcost +. 
		timecostweight *. timecost
	in if !verbose 
			then (E.log "Lookupcost = %f, Opcost = %f, Timecost = %f, Totalcost = %f\n" lookupcost opcost timecost totalcost) 
			else ()
		;
		totalcost

	

(** Runs a hill climbing algorithm to attempt to improve f *)
let hillclimibingoptimiser (fm:funmodule) = 
	Viloptimiser.optimisefunmodule fm;
	Vilannotator.annotateloopprobabilities fm;
	let rec driver f v =
		if !domoduleprint
			then E.log "%s\n" (print_funmodule f)
			else ()
		;
		let rec iterate opts = match opts with
	 		| [] -> 
		 		if !verbose 
					then (E.log "No optimisations remain.\n") 
					else ()
				;
	 			f
	 		| h::t -> let imp = applyoptimisation (Vilcopy.clone f) h
		 		in  Viloptimiser.optimisefunmodule imp;
	 				if !verbose 
						then E.log "Applied %s\n" (string_of_optimisation h)
						else ()
					;
				let impv = evaluate imp
	 			in  if impv < v 
	 				then (
		 				if !verbose 
							then (E.log "Keep optimisation. Recurse\n") 
							else ()
						;
	 					driver imp impv
	 				) else (
		 				if !domoduleprint
							then E.log "%s\n" (print_funmodule imp)
							else ()
						;
	 					if !verbose 
							then (E.log "Discard optimisation. Continue\n") 
							else ()
						;
						iterate t
					)
	 	in iterate (orderoptimisations f (getpossibleoptimisations f))
	 in driver fm (evaluate fm)