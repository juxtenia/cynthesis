open Vil
open Vilevaluator
module E = Errormsg

let verbose = ref false

type optimisationtype =
	| ConditionalExpansion

type optimisation = {
	bid: int;
	desc: optimisationtype;
	apply: funmodule -> funmodule;
}

let string_of_optimisationtype ot = match ot with
	| ConditionalExpansion -> "ConditionalExpansion"

let string_of_optimisation o = "(" ^ string_of_int o.bid ^ "," ^ string_of_optimisationtype o.desc ^ ")"

let isloopwideningtransform (o:optimisation) = match o.desc with
	| _ -> false

(** Gives the possible optimisation steps for f *)
let getpossibleoptimisations (f:funmodule) = [] (*TODO*)

(** Gets the priority of one optimisation over another *)
let getadvantage (f:funmodule) (o1:optimisation) (o2:optimisation) = 
	0 
	(* if o2 will duplicate loop body, of which o1 operates on, then probably to do o1 first *)
	+ if isloopwideningtransform o2 && Vilanalyser.inloopbody f o1.bid o2.bid then 10 else 0

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

let opcostweight = 5.
let timecostweight = 10.

(** Evaluates how good a module is *)
let evaluate (f:funmodule) = 
	Vilannotator.generatescheduleinfo f;
	let opcost = float_of_int (totaloperationcost f)
	in let timecost = weightedtimecost f
	in let totalcost = opcostweight *. opcost +. timecostweight *. timecost
	in if !verbose 
			then (E.log "Opcost = %f, Timecost = %f, Totalcost = %f\n" opcost timecost totalcost) 
			else ()
		;
		totalcost

	

(** Runs a hill climbing algorithm to attempt to improve f *)
let rec hillclimibingoptimiser (f:funmodule) = 
	 let value = evaluate f
	 in let rec driver opts = match opts with
	 	| [] -> 
	 		if !verbose 
				then (E.log "No optimisations remain.\n") 
				else ()
			;
	 		f
	 	| h::t -> let imp = applyoptimisation (Vilcopy.duplicate f) h
	 		in  Viloptimiser.optimisefunmodule imp;
	 			if !verbose 
					then (E.log "Applied %s\n" (string_of_optimisation h)) 
					else ()
				;
	 			if evaluate f > value 
	 			then (
	 				if !verbose 
						then (E.log "Keep optimisation. Recurse\n") 
						else ()
					;
	 				hillclimibingoptimiser imp
	 			) else (
	 				if !verbose 
						then (E.log "Discard optimisation. Continue\n") 
						else ()
					;
					driver t
				)
	 in driver (orderoptimisations f (getpossibleoptimisations f))