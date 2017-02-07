open Vil

let rec update_links ol = List.iter (fun o -> 
	o.operation <- duplicate_voperationtype ol o.operation ) ol
and update_connections ol cl = List.iter (fun c -> 
	c.requires <- (match c.requires with
		| None -> None
		| Some(o,b) -> Some (duplicate_voperationlink ol o,b)
	) ) cl
and duplicate_unop u = u  
and duplicate_binop b = b
and duplicate_funmodule f = {
	vdesc = f.vdesc;
	vinputs = List.map duplicate_vvarinfo f.vinputs;
	vlocals = List.map duplicate_vvarinfo f.vlocals;
	vblocks = List.map duplicate_vblock f.vblocks;
}
and duplicate_vvarinfo v = {
	varname = v.varname;
	vtype = duplicate_vtype v.vtype;
}
and duplicate_vconstinfo c = {
	value = c.value;
	ctype = duplicate_vtype c.ctype;
}
and duplicate_vconnection c = {
	connectfrom = c.connectfrom;
	connectto = c.connectto;
	requires = c.requires; (* This is fixed afterwards! *)
	probability = c.probability;
}
and duplicate_vblock b = let ret = {
	bid = b.bid;
	btype = duplicate_vblocktype b.btype;
	bvars = []; (* We can just run LVA again *)
	bvarexports = [];
	binputs = List.map duplicate_vconnection (List.filter (* remove non entry inputs *)
		(fun c -> match c.connectfrom with
			| None -> true
			| Some _ -> false
		) b.binputs); 
		(* this prevents having to update the operation links and can be
		 * regenerated with Vilannotator.generateconnections *)
	boutputs = List.map duplicate_vconnection b.boutputs;
	bdataFlowGraph = List.map duplicate_voperation b.bdataFlowGraph;
} in update_links ret.bdataFlowGraph; update_connections ret.bdataFlowGraph ret.boutputs; ret
and duplicate_vblocktype b = b
and duplicate_voperationlink ops ol = match ol with
	| Simple (o) -> Simple (List.find (fun o1 -> o1.oid = o.oid) ops)
	| Compound cll -> Compound (List.map (duplicate_vcomplink ops) cll)
and duplicate_vcomplink ops cl = {
	loperation = List.find (fun o1 -> o1.oid = cl.loperation.oid) ops;
	lbase = cl.lbase;
	lwidth = cl.lwidth;
}
and duplicate_voperation o = {
	oid = o.oid;
	operation = o.operation; (* This is fixed afterwards! *)
	ousecount = o.ousecount;
	oschedule = duplicate_vscheduleinfo o.oschedule;
}
and duplicate_vscheduleinfo s = s (* this isn't mutable! *)
and duplicate_voperationtype ops ot = match ot with
	| Variable v -> Variable (duplicate_vvarinfo v)
	| Constant c -> Constant (duplicate_vconstinfo c)
	| Result (v,b,w,ol) -> Result (duplicate_vvarinfo v,b,w,duplicate_voperationlink ops ol)
	| ReturnValue ol -> ReturnValue (duplicate_voperationlink ops ol)
	| Unary (u,ol,t) -> Unary (duplicate_unop u, duplicate_voperationlink ops ol, duplicate_vtype t)
	| Binary (b,ol1,ol2,t) -> Binary (duplicate_binop b, duplicate_voperationlink ops ol1, 
		duplicate_voperationlink ops ol2, duplicate_vtype t)
and duplicate_vtype t = match t with
	| Basic te -> Basic (duplicate_vtypeelement te)
	| Struct (te,cel) -> Struct (duplicate_vtypeelement te, List.map duplicate_vcompelement cel)
	| Union (te,cel) -> Union (duplicate_vtypeelement te, List.map duplicate_vcompelement cel)
and duplicate_vtypeelement te = {
	width = te.width;
	isSigned = te.isSigned;
}
and duplicate_vcompelement ce = {
	ename = ce.ename;
	etype = duplicate_vtype ce.etype;
	ebase = ce.ebase;
}

let duplicate = duplicate_funmodule