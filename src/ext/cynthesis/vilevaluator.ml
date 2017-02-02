open Vil
module E = Errormsg
module M = Matrixutil

(** the cost of having certain operations, 
 *  currently a basic version but can be extended later 
 *)
let operationcost o = operationoffset o

let moduleoperationcost (m:vblock) = 
	List.fold_left (fun t o -> t + operationcost o) 0 m.mdataFlowGraph

let totaloperationcost (f:funmodule) = 
	List.fold_left (fun t m -> t + moduleoperationcost m) 0 f.vblocks

(** the number of time units needed for a module's operations *)
let moduletime = maxtime

(** generate balance equations for a function, solve and multiply 
 *  by the module time *)
let weightedtimecost (f:funmodule) = 
	f.vblocks <- List.sort (fun m1 m2 -> m1.mid - m2.mid) f.vblocks;
	let ids = List.map (fun m -> m.mid) f.vblocks
	in let size = List.length ids
	in let m = M.create_m size size 
	in let v = M.create_v size
	in  (* set the entry point value to be equal to 1 
		 * (using -1 here since this allows inputing 
		 * the probabilities directly)
		 *)	
		M.set_v v 0 (-.1.); 
		(* add -1s down diagonal (as above with -1)*)
		List.iteri (fun i _ -> M.set_m m i i (-.1.)) ids; 
		(* add connections with probability *)
		List.iter (fun m1 -> 
			let pos = Listutil.indexof m1.mid ids 
			in  List.iter (fun c -> match c with
					| {connectto=Some t;probability=p;} -> 
						M.set_m m pos (Listutil.indexof t ids) p
					| _ -> ()
				) m1.moutputs 
		) f.vblocks;
	let s = M.solveaxb m v
	in  (* multiply by clock times in module *)
		List.iteri (fun i m -> 
			M.set_v s i ((M.get_v s i) *. (float_of_int (moduletime m)))
		) f.vblocks;
		(* return the sum *)
		M.sum_v s






