open Vil
module S = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end )
module H = Hashtbl
let initialsize = ref 40

(* code for separating operations *)
type operationcount =
	| Finite of int
	| Infinite
type operationclass = 
	| DSP
	| Lookup of string
	| Notcounted

let dspcount = ref 10

let dodsp = ref true

let getclass (o:voperation) = match o.operation with
	| Lookup(v,_,_) -> Lookup v
	| Binary(Mult,o1,o2,t)
	| Binary(Div,o1,o2,t)
	| Binary(Mod,o1,o2,t) 
		when !dodsp &&
			not (allconst (getlinkchildren o1)) &&
			not (allconst (getlinkchildren o2))
		-> DSP
	| _ -> Notcounted

let countfromclass ll c = match c with
	| DSP -> Finite !dspcount
	| Lookup s -> Finite (Listutil.findfilter (fun l -> 
		if l.lookupname=s then Some l.parrallelcount else None) ll)
	| Notcounted -> Infinite

(* code for filling in the schedule *)
let scheduleat s i u = {
	earliest = s.earliest;
	latest = s.latest;
	set = i;
	assigned = u;
}

let setearliest s i = {
	earliest = i;
	latest = s.latest;
	set = s.set;
	assigned = s.assigned;
}

let setlatest s i = {
	earliest = s.earliest;
	latest = i;
	set = s.set;
	assigned = s.assigned;
}

let asapref (o:voperation) = if o.oschedule.set >=0 then o.oschedule.set else o.oschedule.earliest
let alapref (o:voperation) = if o.oschedule.set >=0 then o.oschedule.set else o.oschedule.latest

(* sets the asap schedule for o *)
let asap (o:voperation) = o.oschedule <- setearliest o.oschedule 
	((List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.earliest) (getchildren o))) + (operationoffset o))

(* sets the alap schedule for o*)
let alap (latest:int) (ops:voperation list) (o:voperation) = o.oschedule <- setlatest o.oschedule (let users = List.filter (fun o1 -> List.memq o (getchildren o1)) ops 
			in match users with
				| [] -> latest
				| h::t -> (List.fold_left min (h.oschedule.latest - (operationoffset h)) (List.map (fun o1 -> o1.oschedule.latest - (operationoffset o1)) t)) 
		)

let reverse_alap (o:voperation) = let alap_v = o.oschedule.latest - (operationoffset o)
	in List.iter (fun o1 ->
		if o1.oschedule.set < 0 && o1.oschedule.latest > alap_v
		then o1.oschedule <- setlatest o1.oschedule alap_v
		else ()
	) (getchildren o)

(* gets classes of things to iterate through *)
let rec childclassesrevorderset (res:voperation list list) (acc:S.t) (ops:voperation list) =
	match ops with
	| [] -> res
	| _ -> let (ready,notready) = List.partition (fun o -> 
			List.for_all (fun c -> S.mem c.oid acc) (getchildren o)) ops
		in let nextset = (S.union (S.of_list (List.map (fun o -> o.oid) ready)) acc)
		in childclassesrevorderset (ready::res) nextset notready	

let childclassesset (res:voperation list list) (acc:S.t) (ops:voperation list) =
	List.rev (childclassesrevorderset res acc ops)

let fastasap (acc:S.t) (ops:voperation list) = 
	let classes = childclassesset [] acc ops
	in List.iter (List.iter asap) classes

let fastalap (latest:int) (acc:S.t) (ops:voperation list) = 
	let classes = childclassesrevorderset [] acc ops
	in  List.iter (fun o -> o.oschedule <- setlatest o.oschedule latest) ops;
		List.iter (List.iter reverse_alap) classes

(* builds asap schedule for ops (start with acc=[]) *)
let rec generateasap (ops:voperation list) = 
	fastasap S.empty ops
(* builds alap schedule for ops (start with acc=[] and latest as the maximum asap value) *)
let rec generatealap (latest:int) (ops:voperation list) = 
	fastalap latest S.empty ops

let compareop o1 o2 = if o1.oschedule.latest-o2.oschedule.latest <> 0
	then o1.oschedule.latest-o2.oschedule.latest
	else o1.oschedule.earliest-o2.oschedule.earliest

let getearliesttime (o:voperation) = 
	(List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.set) (getchildren o))) + (operationoffset o)

let rec schedule ll ops = 
	let sorted = List.sort compareop ops
	in let tbl = H.create !initialsize
	in let classes = childclassesset [] S.empty sorted
	in let getCount (c:operationclass) (i:int) = try H.find tbl (c,i) 
		with | Not_found -> 0
	in let setCount (c:operationclass) (i:int) (cc:int) = H.replace tbl (c,i) cc
	in let rec schedCount o c cc i = 
		let sc = getCount c i
		in  if sc >= cc 
			then schedCount o c cc (i+1)
			else o.oschedule <- scheduleat o.oschedule i sc;
				setCount c i (sc+1)
	in  List.iter (fun c -> 
			List.iter (fun o ->
				let cl = getclass o
				in let i = getearliesttime o
				in match countfromclass ll cl with
				| Infinite -> o.oschedule <- scheduleat o.oschedule i (-1)
				| Finite cc -> schedCount o cl cc i
			) c
		) classes

(* generates schedules for all blocks *)
let generatescheduleinfo (f:funmodule) = 
	dodsp := List.length (Listutil.mapflatten (fun b -> 
		List.filter (fun o -> getclass o = DSP) b.bdataFlowGraph) f.vblocks) > !dspcount;
	List.iter (fun m -> 
		List.iter (fun o -> o.oschedule <- emptyschedule) m.bdataFlowGraph;
		generateasap m.bdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.bdataFlowGraph) m.bdataFlowGraph;
		schedule f.vglobals m.bdataFlowGraph;
	) f.vblocks