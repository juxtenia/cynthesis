open Vil
module S = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end )

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

let rec takefromlist i l = match (i,l) with
	| (0,_) -> ([],l)
	| (_,[]) -> ([],[])
	| (_,h::t) -> let (p,e) = takefromlist (i-1) t
		in ((h,i-1)::p,e)

let compareops o1 o2 = match (o1.oschedule.latest-o2.oschedule.latest,o1.oschedule.earliest-o2.oschedule.earliest) with
	| (0,x) 
	| (x,_) -> x

let getschedulable ll als ol = 
	let rec splitter ac ol = match ol with
		| [] -> ac
		| h::t -> let opclass = getclass h
			in let (m,nm) = List.partition (fun o -> getclass o = opclass) t
			in splitter ((h::m,opclass)::ac) nm
	in let classed = splitter [] ol
	in let sorted = List.map (fun (l,c) -> (List.sort compareops l,c)) classed
	in let rec decider sl = match sl with
		| [] -> ([],[])
		| ([],_)::t -> decider t
		| (f::l,c)::t -> let (s,ns) = decider t
			in match countfromclass ll (getclass f) with
			| Infinite -> ((List.map (fun o -> (o,-1)) (f::l))@s,ns)
			| Finite i -> let count = List.length (List.filter (fun o -> getclass o = c) als)
				in let (ts,nts) = takefromlist (i-count) (f::l)
				in (ts@s,nts@ns)
	in decider sorted

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
	((List.fold_left max 0 (List.map (fun o1 -> asapref o1) (getchildren o))) + (operationoffset o))

(* sets the alap schedule for o*)
let alap (latest:int) (ops:voperation list) (o:voperation) = o.oschedule <- setlatest o.oschedule (let users = List.filter (fun o1 -> List.memq o (getchildren o1)) ops 
			in match users with
				| [] -> latest
				| h::t -> (List.fold_left min ((alapref h) - (operationoffset h)) (List.map (fun o1 -> o1.oschedule.latest - (operationoffset o1)) t)) 
		)

(* gets classes of things to iterate through *)
let rec childclassesrevorder (res:voperation list list) (acc:voperation list) (ops:voperation list) =
	match ops with
	| [] -> res
	| _ -> let (ready,notready) = List.partition (fun o -> childreninlist true o acc) ops
		in childclassesrevorder (ready::res) (List.rev_append ready acc) notready

let rec childclassesrevorderset (acc:voperation list) (ops:voperation list) = 
	let startset = S.of_list (List.map (fun o -> o.oid) acc)
	in let rec driver (res:voperation list list) (acc:S.t) (ops:voperation list) =
		match ops with
		| [] -> res
		| _ -> let (ready,notready) = List.partition (fun o -> 
				List.for_all (fun c -> S.mem c.oid acc) (getchildren o)) ops
			in let nextset = (S.union (S.of_list (List.map (fun o -> o.oid) ready)) acc)
			in driver (ready::res) nextset notready
	in driver [] startset ops

let fastasap (acc:voperation list) (ops:voperation list) = 
	let classes = List.rev (childclassesrevorderset acc ops)
	in List.iter (List.iter asap) classes


let fastalap (latest:int) (acc:voperation list) (ops:voperation list) = 
	let classes = childclassesrevorderset acc ops
	in List.iter (List.iter (alap latest acc)) classes

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

let rec scheduleiterator ll i acc currentstep todo =  
	match todo with
	| [] -> ()
	| x -> match List.partition (fun o -> 
		o.oschedule.earliest <= i && childreninlist true o acc ) todo 
	with
		| ([],_) -> scheduleiterator ll (i+1) acc [] todo
		| (ts,ntodo) -> match getschedulable ll currentstep ts with
			| ([],_) -> let (rs,nt) = List.partition (fun o -> 
					if o.oschedule.earliest<=i 
					then (o.oschedule <- setearliest o.oschedule (i+1); true )
					else false) todo;
				in  fastasap (rs@acc) nt; 
					fastalap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
						0 todo) acc todo;
					scheduleiterator ll (i+1) acc [] todo
			| (s,ns) -> let nowscheduled = (List.map (fun (o,u) -> 
					o.oschedule <- scheduleat o.oschedule i u; o) s)
				in let nextstep = nowscheduled@currentstep
				in let nextacc = nowscheduled@acc
				in let nexttodo = ns@ntodo
				in  fastasap nextacc nexttodo; 
					fastalap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
						0 nexttodo) nextacc nexttodo;
					scheduleiterator ll i nextacc nextstep nexttodo


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
	dodsp := List.length (Listutil.mapflatten (fun b -> 
		List.filter (fun o -> getclass o = DSP) b.bdataFlowGraph) f.vblocks) > !dspcount;
	List.iter (fun m -> 
		List.iter (fun o -> o.oschedule <- emptyschedule) m.bdataFlowGraph;
		generateasap [] m.bdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.bdataFlowGraph) [] m.bdataFlowGraph;
		scheduleiterator f.vglobals 0 [] [] m.bdataFlowGraph;
	) f.vblocks