open Vil

let rec update_links ol = List.iter (fun o -> 
	o.operation <- clone_voperationtype ol o.operation ) ol
and update_connections ol cl = List.iter (fun c -> 
	c.requires <- (match c.requires with
		| None -> None
		| Some(o,b) -> Some (clone_voperationlink ol o,b)
	) ) cl
and clone_unop u = u  
and clone_binop b = b
and clone_funmodule f = {
	vdesc = f.vdesc;
	vinputs = List.map clone_vvarinfo f.vinputs;
	vlocals = List.map clone_vvarinfo f.vlocals;
	vblocks = List.map clone_vblock f.vblocks;
}
and clone_vvarinfo v = {
	varname = v.varname;
	vtype = clone_vtype v.vtype;
}
and clone_vconstinfo c = {
	value = c.value;
	ctype = clone_vtype c.ctype;
}
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
} in update_links ret.bdataFlowGraph; update_connections ret.bdataFlowGraph ret.boutputs; ret
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