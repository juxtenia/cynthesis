open Vil
open Vast
module E = Errormsg

let defaultrange (v:vastvariable) = VARIABLE { variable = v; range=None; }

let onewidetype l = {width=1; isSigned=false; logictype=l; arraytype=[]; }
let onewidevar l n = {name=n; resetto=SINGLE Big_int.zero_big_int; typ=onewidetype l; } 

let vil_to_vast_type (l:vastlogic) (t:vtype) :vasttype = let te = gettypeelement t 
	in { width = te.width; isSigned = te.isSigned; logictype = l; arraytype = []; }

let vil_to_vast_variable (l:vastlogic) (v:vvarinfo) :vastvariable = {
	name = v.varname;
	resetto = SINGLE Big_int.zero_big_int;
	typ = vil_to_vast_type l v.vtype;
}

let vil_to_vast_constant (c:vconstinfo) = {
	value = c.value;
	cwidth = (gettypeelement c.ctype).width;
}

let vil_to_vast_unop (u:unop) (e:vastexpression) = match u with
	| Cast -> e
  	| Neg -> UNARY (UMINUS, e)
  	| BNot -> UNARY (UBNOT, e)
  	| LNot -> UNARY (ULNOT, e)

let vil_to_vast_binop (b:binop) (e1:vastexpression) (e2:vastexpression) = 
	let b1 = match b with
		| PlusA -> PLUS
		| MinusA -> MINUS
		| Mult -> MULT
		| Div -> DIV
		| Mod -> MOD
		| Shiftlt -> LSHIFT 
		| Shiftrt -> RSHIFT
		| Lt -> LT
		| Gt -> GT
		| Le -> LE
		| Ge -> GE
		| Eq -> EQ
		| Ne -> NEQ
		| BAnd -> AND
		| BXor -> XOR
		| BOr -> OR
  	in BINARY (b1,e1,e2)

let addvar (r:vastmodule) (v:vastlval) = 
	r.locals <- v.variable :: r.locals

let addregvar (r:vastmodule) (v:vastlval) (b:bool) (e:vastexpression) = 
	r.locals <- v.variable :: r.locals;
	r.clockedge <- {var=v; assign=e; blocking=b;} :: r.clockedge

let addwirevar (r:vastmodule) (v:vastlval) (b:bool) (e:vastexpression) = 
	r.locals <- v.variable :: r.locals;
	r.always <- ({var=v; assign=e; blocking=b;} :: r.always)

let getinputvariablename (bid:int) (v:vvarinfo) = Printf.sprintf "input_%s_%d" v.varname bid;;
let getinputfollowvariablename (bid:int) (v:vvarinfo) = Printf.sprintf "input_%s_%d_follow" v.varname bid;;
let getoutputvariablename (bid:int) (v:vvarinfo) = Printf.sprintf "output_%s_%d" v.varname bid;;
let startcontrol = "start"
let endcontrol = "end"
let followcontrol = "follow"
let getcontrolvariablename (bid:int) (s:string) = Printf.sprintf "control_%d_%s" bid s;;
let getoperationvariablename (bid:int) (o:voperation) = Printf.sprintf "operation_%d_%d" bid o.oid;;
let getreturnvariablename (bid:int) = Printf.sprintf "return_%d" bid;;

let getinputvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getinputvariablename bid v)
let getinputfollowvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getinputfollowvariablename bid v)
let getoutputvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getoutputvariablename bid v)
let getcontrolstartvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid startcontrol)
let getcontrolendvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid endcontrol)
let getcontrolfollowvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid followcontrol)
let getoperationvariable (r:vastmodule) (bid:int) (o:voperation) = getvar r (getoperationvariablename bid o)
let getreturnvariable (r:vastmodule) (bid:int) = getvar r (getreturnvariablename bid)

let makelvalnorange (n:string) (t:vasttype) = {
		variable={
			name = n;
			resetto = SINGLE Big_int.zero_big_int;
			typ = t;
		};
		range=None
	} 

let makeinputvariable (r:vastmodule) (m:vblock) (v:vvarinfo) = 
	let inputvariable = (makelvalnorange (getinputvariablename m.bid v) 
		(vil_to_vast_type WIRE v.vtype))
	in  addvar r inputvariable;
		addregvar r (makelvalnorange (getinputfollowvariablename m.bid v) 
			(vil_to_vast_type REG v.vtype)) false (VARIABLE inputvariable)

let makeoutputregvariable (r:vastmodule) (m:vblock) (v:vvarinfo) (o:vastexpression) = 
	addregvar r (makelvalnorange (getoutputvariablename m.bid v)
		(vil_to_vast_type REG v.vtype)) false o

let makeoutputwirevariable (r:vastmodule) (m:vblock) (v:vvarinfo) (o:vastexpression) = 
	addwirevar r (makelvalnorange (getoutputvariablename m.bid v)
		(vil_to_vast_type WIRE v.vtype)) true o

let makecontrolvariable (l:vastlogic) (r:vastmodule) (m:vblock) = 
	let controlstartvariable = makelvalnorange (getcontrolvariablename m.bid startcontrol) 
		(onewidetype l)
	in let controlendvariable = makelvalnorange (getcontrolvariablename m.bid endcontrol) 
		(onewidetype WIRE)
	in let controlfollowvariable = makelvalnorange (getcontrolvariablename m.bid followcontrol)
		(onewidetype REG)
	in  addvar r controlstartvariable;
		addwirevar r controlendvariable true (VARIABLE controlstartvariable);
		addregvar r controlfollowvariable (match l with
			| WIRE -> true
			| REG -> false
			| NA -> E.s (E.error "Variable %s can't have NA logic type" 
				controlstartvariable.variable.name)
		) (VARIABLE controlendvariable)

let makecontrolvariablesequence (i:int) (r:vastmodule) (m:vblock) = 
	let controlstartvariable = makelvalnorange (getcontrolvariablename m.bid startcontrol) 
		(onewidetype REG)
	in let controlendvariable = makelvalnorange (getcontrolvariablename m.bid endcontrol) 
		(onewidetype WIRE)
	in let controlfollowvariable = makelvalnorange (getcontrolvariablename m.bid followcontrol)
		(onewidetype REG)
	in let rec driver j p = if j >= i then p else(
		let controlnextvariable = makelvalnorange (getcontrolvariablename m.bid (string_of_int j)) 
			(onewidetype REG)
		in addregvar r controlnextvariable false (VARIABLE p);
		driver (j+1) controlnextvariable
	)
	in let lastreg = driver 1 controlstartvariable
	in 	addvar r controlstartvariable;
		addwirevar r controlendvariable true (VARIABLE lastreg);
		addregvar r controlfollowvariable false (VARIABLE controlendvariable)

let makeoperationwirevariable (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) (ov:vastexpression)= 
	addwirevar r (makelvalnorange (getoperationvariablename m.bid o) 
		(vil_to_vast_type WIRE (gettype v o))) true ov

let makeoperationregvariable (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) (ov:vastexpression) = 
	addregvar r (makelvalnorange (getoperationvariablename m.bid o) 
		(vil_to_vast_type REG (gettype v o))) false ov

let makereturnregvariable (r:vastmodule) (v:vvarinfo) (m:vblock) (o:vastexpression) =
	addregvar r (makelvalnorange (getreturnvariablename m.bid) 
		(vil_to_vast_type REG v.vtype)) false o

let makereturnwirevariable (r:vastmodule) (v:vvarinfo) (m:vblock) (o:vastexpression) =
	addwirevar r (makelvalnorange (getreturnvariablename m.bid) 
		(vil_to_vast_type WIRE v.vtype)) true o

let refertocomplink (r:vastmodule) (bid:int) (cl:vcomplink) = 
	VARIABLE {variable=getoperationvariable r bid cl.loperation; range=Some(cl.lbase+cl.lwidth-1,cl.lbase)}

let refertooperation (r:vastmodule) (bid:int) (ol:voperationlink) = match ol with
	| Simple o -> defaultrange (getoperationvariable r bid o)
	| Compound cel-> CONCAT (List.map (refertocomplink r bid) cel)

let makeoperation (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) = match o.operation with
	| ReturnValue o1 -> makereturnwirevariable r v m (refertooperation r m.bid o1)
	| Result(var,_,_,o1) -> makeoutputwirevariable r m var (refertooperation r m.bid o1)
	| Variable v -> makeoperationwirevariable r v m o (defaultrange (getinputvariable r m.bid v))
	| Constant c-> makeoperationwirevariable r v m o (CONST (vil_to_vast_constant c))
	| Unary (Cast,o1,t) -> makeoperationwirevariable r v m o (vil_to_vast_unop Cast
		(refertooperation r m.bid o1))
	| Unary (u,o1,t) -> makeoperationregvariable r v m o (vil_to_vast_unop u 
		(refertooperation r m.bid o1))
	| Binary(b,o1,o2,t) -> makeoperationregvariable r v m o (vil_to_vast_binop b 
		(refertooperation r m.bid o1) (refertooperation r m.bid o2))
	| Ternary(o1,o2,o3,t) -> makeoperationregvariable r v m o (TERNARY (
		refertooperation r m.bid o1, refertooperation r m.bid o2, refertooperation r m.bid o3))

let makeoperationwire (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) = match o.operation with
	| Unary (u,o1,t) -> makeoperationwirevariable r v m o (vil_to_vast_unop u 
		(refertooperation r m.bid o1))
	| Binary(b,o1,o2,t) -> makeoperationwirevariable r v m o (vil_to_vast_binop b 
		(refertooperation r m.bid o1) (refertooperation r m.bid o2))
	| Ternary(o1,o2,o3,t) -> makeoperationwirevariable r v m o (TERNARY (
		refertooperation r m.bid o1, refertooperation r m.bid o2, refertooperation r m.bid o3))
	| ReturnValue _ 
	| Result(_,_,_,_) 
	| Variable _ 
	| Constant _-> makeoperation r v m o

let makeoperationlatchvariables (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) = match o.operation with
	| ReturnValue o1 -> makereturnregvariable r v m (refertooperation r m.bid o1)
	| Result(var,_,_,o1) -> makeoutputregvariable r m var (refertooperation r m.bid o1)
	| Variable _ 
	| Constant _
	| Unary (_,_,_) 
	| Binary(_,_,_,_) 
	| Ternary(_,_,_,_) -> makeoperationwire r v m o

let makeanoperation (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) = 
	if maxtime m = o.oschedule.set && List.exists (fun o1 -> o1.oid=o.oid) 
		(Listutil.mapflatten getlinkchildren (getswitches m))
	then makeoperationwire r v m o
	else makeoperation r v m o

let rec makeoperations (makeop:vastmodule -> vvarinfo -> vblock -> voperation -> unit) 
	(r:vastmodule) (v:vvarinfo) (m:vblock) (acc:voperation list) 
		(skip:voperation list) (ops:voperation list) = 
	match (ops,skip) with
	| ([],[]) -> ()
	| ([],_ ) -> makeoperations makeop r v m acc [] skip
	| (h::t,_) -> if(childreninlist true h acc)
		then
			(makeop r v m h;
			makeoperations makeop r v m (h::acc) skip t)
		else makeoperations makeop r v m acc (h::skip) t

let iszerotime (m:vblock) = 
	maxtime m = 0 && List.length m.boutputs < 2

let zero_time_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	List.iter (makeinputvariable r m) m.bvars;
	makecontrolvariable WIRE r m;
	makeoperations makeoperationwire r v m [] [] m.bdataFlowGraph;
	let remainingoutputvars = List.filter (fun v -> not (hasvariableresult v m.bdataFlowGraph)) m.bvarexports
	in  List.iter (fun v -> 
			makeoutputwirevariable r m v (
				defaultrange (getinputvariable r m.bid v)
			)) remainingoutputvars	

let one_clock_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	List.iter (makeinputvariable r m) m.bvars;
	makecontrolvariable REG r m;
	makeoperations makeoperationlatchvariables r v m [] [] m.bdataFlowGraph;
	let remainingoutputvars = List.filter (fun v -> not (hasvariableresult v m.bdataFlowGraph)) m.bvarexports
	in  List.iter (fun v -> 
			makeoutputregvariable r m v (
				defaultrange (getinputvariable r m.bid v)
			)) remainingoutputvars	

let positive_time_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	List.iter (makeinputvariable r m) m.bvars;
	makecontrolvariablesequence (maxtime m) r m;
	makeoperations makeanoperation r v m [] [] m.bdataFlowGraph;
	let remainingoutputvars = List.filter (fun v -> not (hasvariableresult v m.bdataFlowGraph)) m.bvarexports
	in  List.iter (fun v -> 
			makeoutputwirevariable r m v (
				defaultrange (getinputvariable r m.bid v)
			)) remainingoutputvars	

let vil_to_vast_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	if iszerotime m
	then zero_time_module r v m
	else if maxtime m <= 1
	then one_clock_module r v m
	else positive_time_module r v m
	
let addclocked (r:vastmodule) (v:vastvariable) (e:vastexpression) = 
	r.clockedge <- { var={ variable=v; range=None; };
		assign=e; blocking=false; } :: r.clockedge

let addalways (r:vastmodule) (v:vastvariable) (e:vastexpression) = 
	r.always <- { var={ variable=v; range=None; };
		assign=e; blocking=true; } :: r.always

let startinput = "start"
let startfollow = "startfollow"
let finishoutput = "finish"
let zeroconst = {value = Big_int.zero_big_int; cwidth=1;}

let getexpfromconnection (r:vastmodule) (c:vconnection) = match c with
	| {connectfrom=None;} -> 
		BINARY (LAND, defaultrange (getvar r startinput), 
			UNARY (ULNOT, defaultrange (getvar r startfollow)))
	| {connectfrom=Some i; requires=None} ->
		defaultrange (getcontrolendvariable r i)
	| {connectfrom=Some i; requires=Some (o,true)} ->
		BINARY (LAND,
			defaultrange (getcontrolendvariable r i),
			refertooperation r i o
		)
	| {connectfrom=Some i; requires=Some (o,false)} ->
		BINARY (LAND,
			defaultrange (getcontrolendvariable r i),
			UNARY(ULNOT,refertooperation r i o)
		)

let dataconnections (r:vastmodule) (m:vblock) = 
	List.iter (fun v -> addalways r (getinputvariable r m.bid v) 
		(List.fold_left (fun a b -> 
			let (ifex,trueex) = match b with
				| {connectfrom=None} -> (
					defaultrange (getvar r startinput),
					defaultrange (getvar r v.varname))
				| {connectfrom=Some i} -> (
					defaultrange (getcontrolfollowvariable r i),
					defaultrange (getoutputvariable r i v))
			in TERNARY (ifex,trueex,a)
		) (defaultrange (getinputfollowvariable r m.bid v)) m.binputs)) m.bvars

let oneconnection (r:vastmodule) (m:vblock) (c:vconnection) = 
	let addwhere = if iszerotime m then addalways else addclocked
	in 
	addwhere r (getcontrolstartvariable r m.bid) 
		(getexpfromconnection r c)

let manyconnections (r:vastmodule) (m:vblock) = 
	let addwhere = if iszerotime m then addalways else addclocked
	in 
	addwhere r (getcontrolstartvariable r m.bid) 
		(List.fold_left (fun a b -> BINARY (LOR,a,getexpfromconnection r b)) 
			(CONST zeroconst) m.binputs)

let returnconnections (r:vastmodule) (f:funmodule) = 
	let returnpoints = List.flatten (List.map 
		(fun m -> Listutil.mapfilter (fun c -> match c with
		| {connectfrom=Some i;connectto=None;requires=r} -> Some (i,r)
		| _ -> None) m.boutputs) f.vblocks)
	in  addclocked r (getvar r finishoutput) (BINARY(LAND,
		UNARY(ULNOT, BINARY (LAND, defaultrange (getvar r startinput), 
			UNARY (ULNOT, defaultrange (getvar r startfollow)))),
		(List.fold_left (fun a (i,re) -> BINARY (LOR,a,
			match re with
				| None -> defaultrange (getcontrolendvariable r i)
				| Some (o,true) -> BINARY (LAND,
					defaultrange (getcontrolendvariable r i),
					refertooperation r i o
				)
				| Some (o,false) -> BINARY (LAND,
					defaultrange (getreturnvariable r i),
					UNARY(ULNOT,refertooperation r i o)
				))) 
			(defaultrange (getvar r finishoutput)) returnpoints)));
		addclocked r (getvar r f.vdesc.varname) 
		(List.fold_left (fun a (i,_) -> 
			TERNARY (
				defaultrange (getcontrolfollowvariable r i),
				defaultrange (getreturnvariable r i),
				a
			)) (defaultrange (getvar r f.vdesc.varname)) returnpoints)


let vil_to_vast_connections (r:vastmodule) (m:vblock) = 
	(match m.binputs with
		| [c] -> oneconnection r m c
		| _ -> manyconnections r m
	);
	dataconnections r m

let vil_to_vast (f:funmodule):vastmodule = 
	let startcontrol = onewidevar NA startinput
	in let startfollowcontrol = onewidevar REG startfollow
	in let readycontrol = onewidevar REG finishoutput
	in let resultoutput = vil_to_vast_variable REG f.vdesc
	in let ret = {
		modname = f.vdesc.varname;
		inputs = startcontrol :: (List.map (vil_to_vast_variable NA) f.vinputs);
		outputs = readycontrol :: resultoutput :: [];
		locals = startfollowcontrol :: [];
		always = [];
		clockedge = {var={variable=startfollowcontrol; range=None; }; 
			assign=defaultrange startcontrol; blocking=false; } :: [];
	}
	in  List.iter (vil_to_vast_module ret f.vdesc) f.vblocks;
		List.iter (vil_to_vast_connections ret) f.vblocks;
		returnconnections ret f;
		ret;;