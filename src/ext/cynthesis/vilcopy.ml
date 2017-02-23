open Vil

let rec update_clone_links ol = List.iter (fun o -> 
	o.operation <- clone_voperationtype ol o.operation ) ol
and update_clone_connections ol cl = List.iter (fun c -> 
	c.requires <- (match c.requires with
		| None -> None
		| Some(o,b) -> Some (clone_voperationlink ol o,b)
	) ) cl
and clone_unop u = u  
and clone_binop b = b
and clone_funmodule f = {
	vdesc = f.vdesc;
	vglobals = List.map clone_vlookupinfo f.vglobals;
	vinputs = List.map clone_vvarinfo f.vinputs;
	vlocals = List.map clone_vvarinfo f.vlocals;
	vblocks = List.map clone_vblock f.vblocks;
}
and clone_vlookupinfo l = {
	lookupname = l.lookupname;
	initialiser = clone_vinitinfo l.initialiser;
	parrallelcount = l.parrallelcount;
}
and clone_vvarinfo v = {
	varname = v.varname;
	vtype = clone_vtype v.vtype;
}
and clone_vconstinfo c = {
	value = c.value;
	ctype = clone_vtype c.ctype;
}
and clone_vinitinfo i = match i with 
	| Const c -> Const (clone_vconstinfo c)
	| Comp l -> Comp (List.map (fun (s,i) -> (s,clone_vinitinfo i)) l)
	| Array l -> Array (List.map clone_vinitinfo l)
and clone_vconnection c = {
	connectfrom = c.connectfrom;
	connectto = c.connectto;
	requires = c.requires; (* This is fixed afterwards! *)
	probability = c.probability;
}
and clone_vblock b = let ret = {
	bid = b.bid;
	btype = clone_vblocktype b.btype;
	bvars = []; (* We can just run LVA again *)
	bvarexports = [];
	binputs = List.map clone_vconnection (List.filter (* remove non entry inputs *)
		(fun c -> match c.connectfrom with
			| None -> true
			| Some _ -> false
		) b.binputs); 
		(* this prevents having to update the operation links and can be
		 * regenerated with Vilannotator.generateconnections *)
	boutputs = List.map clone_vconnection b.boutputs;
	bdataFlowGraph = List.map clone_voperation b.bdataFlowGraph;
} in update_clone_links ret.bdataFlowGraph; 
	update_clone_connections ret.bdataFlowGraph ret.boutputs; 
	ret
and clone_vblocktype b = b
and clone_voperationlink ops ol = match ol with
	| Simple (o) -> Simple (List.find (fun o1 -> o1.oid = o.oid) ops)
	| Compound cll -> Compound (List.map (clone_vcomplink ops) cll)
and clone_vcomplink ops cl = {
	loperation = List.find (fun o1 -> o1.oid = cl.loperation.oid) ops;
	lbase = cl.lbase;
	lwidth = cl.lwidth;
}
and clone_voperation o = {
	oid = o.oid;
	operation = o.operation; (* This is fixed afterwards! *)
	ousecount = o.ousecount;
	oschedule = clone_vscheduleinfo o.oschedule;
}
and clone_vscheduleinfo s = s (* this isn't mutable! *)
and clone_voperationtype ops ot = match ot with
	| Variable v -> Variable (clone_vvarinfo v)
	| Constant c -> Constant (clone_vconstinfo c)
	| Result (v,b,w,ol) -> Result (clone_vvarinfo v,b,w,clone_voperationlink ops ol)
	| ReturnValue ol -> ReturnValue (clone_voperationlink ops ol)
	| Unary (u,ol,t) -> Unary (clone_unop u, clone_voperationlink ops ol, clone_vtype t)
	| Binary (b,ol1,ol2,t) -> Binary (clone_binop b, clone_voperationlink ops ol1, 
		clone_voperationlink ops ol2, clone_vtype t)
	| Ternary (ol1,ol2,ol3,t) -> Ternary (clone_voperationlink ops ol1, clone_voperationlink ops ol2,
		clone_voperationlink ops ol3, clone_vtype t)
	| Lookup (v,ol1,t) -> Lookup (v, List.map (clone_voperationlink ops) ol1, clone_vtype t)
and clone_vtype t = match t with
	| Basic te -> Basic (clone_vtypeelement te)
	| Struct (te,cel) -> Struct (clone_vtypeelement te, List.map clone_vcompelement cel)
	| Union (te,cel) -> Union (clone_vtypeelement te, List.map clone_vcompelement cel)
and clone_vtypeelement te = {
	width = te.width;
	isSigned = te.isSigned;
}
and clone_vcompelement ce = {
	ename = ce.ename;
	etype = clone_vtype ce.etype;
	ebase = ce.ebase;
}

let clone = clone_funmodule



let rec update_duplicate_links reps ol = List.iter (fun o -> 
	o.operation <- duplicate_voperationtype reps ol o.operation ) ol
and update_duplicate_connections reps ol cl = List.iter (fun c -> 
	c.requires <- (match c.requires with
		| None -> None
		| Some(o,b) -> Some (duplicate_voperationlink reps ol o,b)
	) ) cl
and duplicate_unop u = u  
and duplicate_binop b = b
and duplicate_vblocks all bs = 
	let max = List.fold_left (fun a b -> max a b.bid) 0 all
	in let ret = List.map (duplicate_vblock max) bs
	in let modids = List.map (fun b -> b.bid) bs
	in let swap iopt = match iopt with
				| None -> None
				| Some i -> if List.mem i modids then Some (i+max) else Some i
	in  List.iter (fun b -> 
			List.iter (fun c ->
				c.connectto <- swap c.connectto;
				c.connectfrom <- swap c.connectfrom
			) b.boutputs) ret;
		(max,ret)
and duplicate_vvarinfo v = clone_vvarinfo v
and duplicate_vconstinfo c = clone_vconstinfo c
and duplicate_vconnection c = {
	connectfrom = c.connectfrom;
	connectto = c.connectto;
	requires = c.requires; (* This is fixed afterwards! *)
	probability = c.probability;
}
and duplicate_vblock m b = 
	let (reps,ops) = (List.split (List.map duplicate_voperation b.bdataFlowGraph))
	in let ret = {
		bid = b.bid + m;
		btype = duplicate_vblocktype b.btype;
		bvars = []; (* We can just run LVA again *)
		bvarexports = [];
		binputs = []; (* Remove all inputs. We assume no entry points since there should only be one of these. *)
			(* this prevents having to update the operation links and can be
		 	* regenerated with Vilannotator.generateconnections *)
		boutputs = List.map duplicate_vconnection b.boutputs;
		bdataFlowGraph = ops;
	} in update_duplicate_links reps ret.bdataFlowGraph; 
		update_duplicate_connections reps ret.bdataFlowGraph ret.boutputs; 
		ret
and duplicate_vblocktype b = b
and duplicate_voperationlink reps ops ol = match ol with
	| Simple (o) -> let target = snd (List.find (fun (a,_) -> a = o.oid) reps) 
		in Simple (List.find (fun o1 -> o1.oid = target) ops)
	| Compound cll -> Compound (List.map (duplicate_vcomplink reps ops) cll)
and duplicate_vcomplink reps ops cl = 
	let target = snd (List.find (fun (a,_) -> a = cl.loperation.oid) reps) 
	in {
		loperation = List.find (fun o1 -> o1.oid = target) ops;
		lbase = cl.lbase;
		lwidth = cl.lwidth;
	}
and duplicate_voperation o = 
	let id = getnewid ()
	in ((o.oid,id),{
		oid = id;
		operation = o.operation; (* This is fixed afterwards! *)
		ousecount = o.ousecount;
		oschedule = emptyschedule;
	})
and duplicate_vscheduleinfo s = s (* this isn't mutable! *)
and duplicate_voperationtype reps ops ot = match ot with
	| Variable v -> Variable (duplicate_vvarinfo v)
	| Constant c -> Constant (duplicate_vconstinfo c)
	| Result (v,b,w,ol) -> Result (duplicate_vvarinfo v,b,w,duplicate_voperationlink reps ops ol)
	| ReturnValue ol -> ReturnValue (duplicate_voperationlink reps ops ol)
	| Unary (u,ol,t) -> Unary (duplicate_unop u, duplicate_voperationlink reps ops ol, duplicate_vtype t)
	| Binary (b,ol1,ol2,t) -> Binary (duplicate_binop b, duplicate_voperationlink reps ops ol1, 
		duplicate_voperationlink reps ops ol2, duplicate_vtype t)
	| Ternary (ol1,ol2,ol3,t) -> Ternary (duplicate_voperationlink reps ops ol1, duplicate_voperationlink reps ops ol2,
		duplicate_voperationlink reps ops ol3, duplicate_vtype t)
	| Lookup (v,ol1,t) -> Lookup (v,
		List.map (duplicate_voperationlink reps ops) ol1, duplicate_vtype t)
and duplicate_vtype t = clone_vtype t;;

let duplicate = duplicate_vblocks