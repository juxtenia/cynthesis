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
				| h::t -> (List.fold_left min ((alapref h) - (operationoffset h)) (List.map (fun o1 -> (alapref o1) - (operationoffset o1)) t)) 
		)

let reverse_alap (o:voperation) = let alap_v = (alapref o) - (operationoffset o)
	in List.iter (fun o1 ->
		if o1.oschedule.set < 0 && o1.oschedule.latest > alap_v
		then o1.oschedule <- setlatest o1.oschedule alap_v
		else ()
	) (getchildren o)

(* gets classes of things to iterate through *)
let rec childclasses (res:voperation list list) (acc:voperation list) (ops:voperation list) =
	match ops with
	| [] -> res
	| _ -> let (ready,notready) = List.partition (fun o -> childreninlist true o acc) ops
		in childclasses (ready::res) (List.rev_append ready acc) notready

let rec childclassesset (res:voperation list list) (acc:S.t) (ops:voperation list) =
	match ops with
	| [] -> List.rev res
	| _ -> let (ready,notready) = List.partition (fun o -> 
			List.for_all (fun c -> S.mem c.oid acc) (getchildren o)) ops
		in let nextset = (S.union (S.of_list (List.map (fun o -> o.oid) ready)) acc)
		in childclassesset (ready::res) nextset notready
	

let fastasap (acc:S.t) (ops:voperation list) = 
	let classes = childclassesset [] acc ops
	in List.iter (List.iter asap) classes


let fastalap (latest:int) (acc:S.t) (ops:voperation list) = 
	let classes = childclassesset [] acc ops
	in  List.iter (fun o -> o.oschedule <- setlatest o.oschedule latest) ops;
		List.iter (List.iter reverse_alap) classes

let fastasapalap (acc:S.t) (ops:voperation list) = 
	let classes = childclassesset [] acc ops
	in let rec asapdriver cl = match cl with
		| [] -> 0
		| [c] -> List.iter asap c;
			(List.fold_left (fun a b -> max a b.oschedule.earliest) 0 c)
		| h::t -> List.iter asap h;
			asapdriver t
	in let latest = asapdriver classes
	in  List.iter (fun o -> o.oschedule <- setlatest o.oschedule latest) ops;
		List.iter (List.iter reverse_alap) classes

(* builds asap schedule for ops (start with acc=[]) *)
let rec generateasap (acc:voperation list) (ops:voperation list) = 
	fastasap (S.of_list (List.map (fun o -> o.oid) acc)) ops
(* builds alap schedule for ops (start with acc=[] and latest as the maximum asap value) *)
let rec generatealap (latest:int) (acc:voperation list) (ops:voperation list) = 
	fastalap latest (S.of_list (List.map (fun o -> o.oid) acc)) ops

let compareop o1 o2 = if o1.oschedule.latest-o2.oschedule.latest <> 0
	then o1.oschedule.latest-o2.oschedule.latest
	else o1.oschedule.earliest-o2.oschedule.earliest

let getearliesttime (o:voperation) = 
	(List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.set) (getchildren o))) + (operationoffset o)

let rec lineariterator ll ops = 
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
				| Finite c -> schedCount o cl c i
			) c
		) classes

let rec scheduleiterator ll i acc currentstep todo =  
	match todo with
	| [] -> ()
	| x -> match List.partition (fun o -> 
			o.oschedule.earliest <= i &&
			List.for_all (fun c -> S.mem c.oid acc) (getchildren o)
		) todo
	with
		| ([],_) -> scheduleiterator ll (i+1) acc [] todo
		| (ts,ntodo) -> match getschedulable ll currentstep ts with
			| ([],_) -> let (rs,nt) = List.partition (fun o -> 
					if o.oschedule.earliest<=i 
					then (o.oschedule <- setearliest o.oschedule (i+1); true )
					else false) todo;
				in  fastasapalap (S.union (S.of_list (List.map (fun o -> o.oid) rs)) acc) nt; 
					scheduleiterator ll (i+1) acc [] todo
			| (s,ns) -> let nowscheduled = (List.map (fun (o,u) -> 
					o.oschedule <- scheduleat o.oschedule i u; o) s)
				in let nextstep = nowscheduled@currentstep
				in let nextacc = (S.union (S.of_list (List.map (fun o -> o.oid) nowscheduled)) acc)
				in let nexttodo = ns@ntodo
				in  fastasapalap nextacc nexttodo; 
					scheduleiterator ll i nextacc nextstep nexttodo

(* generates schedules for all blocks *)
let generatescheduleinfo (f:funmodule) = 
	dodsp := List.length (Listutil.mapflatten (fun b -> 
		List.filter (fun o -> getclass o = DSP) b.bdataFlowGraph) f.vblocks) > !dspcount;
	List.iter (fun m -> 
		List.iter (fun o -> o.oschedule <- emptyschedule) m.bdataFlowGraph;
		generateasap [] m.bdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.bdataFlowGraph) [] m.bdataFlowGraph;
		lineariterator f.vglobals m.bdataFlowGraph;
	) f.vblocks