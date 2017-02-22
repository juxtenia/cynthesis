open Vil

(* code for filling in the schedule *)

(* sets the asap schedule for o *)
let asap (o:voperation) = o.oschedule <- 
	{
		earliest = (List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.earliest) (getchildren o))) + (operationoffset o);
		latest = o.oschedule.latest; 
		set = o.oschedule.set;
		assigned = o.oschedule.assigned;
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
		assigned = o.oschedule.assigned;
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
		{
			earliest = o.oschedule.earliest;
			latest = o.oschedule.latest;
			set = o.oschedule.earliest;
			assigned = o.oschedule.assigned;
		}
	) m.bdataFlowGraph

(* generates schedules for all blocks *)
let generatescheduleinfo (f:funmodule) = 
	List.iter (fun m -> 
		generateasap [] m.bdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.bdataFlowGraph) [] m.bdataFlowGraph;
		generateschedule m;
	) f.vblocks