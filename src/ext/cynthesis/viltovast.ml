open Vil
open Vast
module E = Errormsg

let defaultrange (v:vastvariable) = VARIABLE { variable = v; range=None; }

let vil_to_vast_type (l:vastlogic) (t:vtype) :vasttype = {
	width = t.width;
	isSigned = t.isSigned;
	logictype = l;
}

let vil_to_vast_variable (l:vastlogic) (v:vvarinfo) :vastvariable = {
	name = v.varname;
	resetto = Big_int.zero_big_int;
	typ = vil_to_vast_type l v.vtype;
}

let vil_to_vast_constant (c:vconstinfo) = {
	value = c.value;
	cwidth = c.ctype.width;
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

let getinputvariablename (mid:int) (v:vvarinfo) = Printf.sprintf "input_%s_%d" v.varname mid;;
let getoutputvariablename (mid:int) (v:vvarinfo) = Printf.sprintf "output_%s_%d" v.varname mid;;
let maincontrol = ""
let followcontrol = "follow"
let getcontrolvariablename (mid:int) (s:string) = Printf.sprintf "control_%d_%s" mid s;;
let getoperationvariablename (mid:int) (o:voperation) = Printf.sprintf "operation_%d_%d" mid o.oid;;
let getreturnvariablename (mid:int) = Printf.sprintf "return_%d" mid;;

let getinputvariable (r:vastmodule) (mid:int) (v:vvarinfo) = getvar r (getinputvariablename mid v)
let getoutputvariable (r:vastmodule) (mid:int) (v:vvarinfo) = getvar r (getoutputvariablename mid v)
let getcontrolvariable (r:vastmodule) (mid:int) (s:string) = getvar r (getcontrolvariablename mid s)
let getoperationvariable (r:vastmodule) (mid:int) (o:voperation) = getvar r (getoperationvariablename mid o)
let getreturnvariable (r:vastmodule) (mid:int) = getvar r (getreturnvariablename mid)

let makeinputvariable (r:vastmodule) (m:vmodule) (v:vvarinfo) = 
	addvar r {
		variable={
			name = getinputvariablename m.mid v;
			resetto = Big_int.zero_big_int;
			typ = vil_to_vast_type WIRE v.vtype;
		};
		range=None
	} 

let rec makeexp (r:vastmodule) (m:vmodule) (o:voperation) = match o.operation with
	| Variable v -> VARIABLE {variable=(getinputvariable r m.mid v); range = None}
	| Constant c -> CONST (vil_to_vast_constant c)
	| Unary (u,o1,_) -> vil_to_vast_unop u (makeexp r m o1)
	| Binary (b,o1,o2,_) -> vil_to_vast_binop b (makeexp r m o1) (makeexp r m o2)
	| _ -> E.s (E.error "Unexpected operation %s\n" (string_of_voperation o))

let makeoutputvariable (r:vastmodule) (m:vmodule) (v:vvarinfo) (o:voperation) = 
	addregvar r {
		variable={
			name = getoutputvariablename m.mid v;
			resetto = Big_int.zero_big_int;
			typ = vil_to_vast_type REG v.vtype;
		};
		range=None;
	} false (makeexp r m o)

let makecontrolvariable (r:vastmodule) (m:vmodule) = 
	let controlvariable = { variable={
			name=getcontrolvariablename m.mid maincontrol;
			resetto = Big_int.zero_big_int;
			typ={width=1; isSigned=false; logictype=REG;}
		}; range=None; }
	in let controlfollowvariable = { variable={
			name=getcontrolvariablename m.mid followcontrol;
			resetto = Big_int.zero_big_int;
			typ={width=1; isSigned=false; logictype=REG;}
		}; range=None; }
	in  addvar r controlvariable;
		addregvar r controlfollowvariable false (VARIABLE controlvariable)

let makeoperationwirevariable (r:vastmodule) (m:vmodule) (o:voperation) = 
	addwirevar r {
		variable={
			name=getoperationvariablename m.mid o;
			resetto=Big_int.zero_big_int;
			typ=vil_to_vast_type WIRE (gettype o);
		}; range=None; } true (makeexp r m o)

let makereturnvariable (r:vastmodule) (v:vvarinfo) (m:vmodule) (o:voperation) =
	addregvar r {
		variable={
			name=getreturnvariablename m.mid;
			resetto=Big_int.zero_big_int;
			typ=vil_to_vast_type REG v.vtype;
		};range=None } false (makeexp r m o)

let vil_to_vast_module (r:vastmodule) (v:vvarinfo) (m:vmodule) =
	List.iter (makeinputvariable r m) m.mvars;
	makecontrolvariable r m;
	List.iter (makeoperationwirevariable r m) (getswitches m);
	let outputvars = ref m.mvarexports
	in  List.iter (fun o -> match o.operation with
			| Result(v,o1) -> 
				outputvars := List.filter (fun v1 -> v1.varname <> v.varname) !outputvars;
				makeoutputvariable r m v o1
			| ReturnValue o1 -> makereturnvariable r v m o1
			| _ -> ()) m.mdataFlowGraph;
		List.iter (fun v -> 
			makeoutputvariable r m v (
				{ oid= -1; ousecount=0; oschedule=emptyschedule; operation=Variable v; }
			)) !outputvars	
	
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
		defaultrange (getcontrolvariable r i maincontrol)
	| {connectfrom=Some i; requires=Some (o,true)} ->
		BINARY (LAND,
			defaultrange (getcontrolvariable r i maincontrol),
			defaultrange (getoperationvariable r i o)
		)
	| {connectfrom=Some i; requires=Some (o,false)} ->
		BINARY (LAND,
			defaultrange (getcontrolvariable r i maincontrol),
			UNARY(ULNOT,defaultrange (getoperationvariable r i o))
		)

let oneconnection (r:vastmodule) (m:vmodule) (c:vconnection) = 
	addclocked r (getcontrolvariable r m.mid maincontrol) 
		(getexpfromconnection r c);
	List.iter (fun v -> addalways r (getinputvariable r m.mid v) 
		(match c with
			| {connectfrom=None} -> defaultrange (getvar r v.varname)
			| {connectfrom=Some i} -> defaultrange (getoutputvariable r i v)
		)) m.mvars

let manyconnections (r:vastmodule) (m:vmodule) = 
	addclocked r (getcontrolvariable r m.mid maincontrol) 
		(List.fold_left (fun a b -> BINARY (LOR,a,getexpfromconnection r b)) 
			(CONST zeroconst) m.minputs);
	List.iter (fun v -> addalways r (getinputvariable r m.mid v) 
		(List.fold_left (fun a b -> 
			let (ifex,trueex) = match b with
				| {connectfrom=None} -> (
					defaultrange (getvar r startinput),
					defaultrange (getvar r v.varname))
				| {connectfrom=Some i} -> (
					defaultrange (getcontrolvariable r i followcontrol),
					defaultrange (getoutputvariable r i v))
			in TERNARY (ifex,trueex,a)
		) (CONST zeroconst) m.minputs)) m.mvars

let returnconnections (r:vastmodule) (f:funmodule) = 
	let returnpoints = List.flatten (List.map 
		(fun m -> Listutil.mapfilter (fun c -> match c with
		| {connectfrom=Some i;connectto=None;requires=r} -> Some (i,r)
		| _ -> None) m.moutputs) f.vmodules)
	in  addclocked r (getvar r finishoutput) (BINARY(LAND,
		UNARY(ULNOT, BINARY (LAND, defaultrange (getvar r startinput), 
			UNARY (ULNOT, defaultrange (getvar r startfollow)))),
		(List.fold_left (fun a (i,re) -> BINARY (LOR,a,
			match re with
				| None -> defaultrange (getcontrolvariable r i maincontrol)
				| Some (o,true) -> BINARY (LAND,
					defaultrange (getcontrolvariable r i maincontrol),
					defaultrange (getoperationvariable r i o)
				)
				| Some (o,false) -> BINARY (LAND,
					defaultrange (getreturnvariable r i),
					UNARY(ULNOT,defaultrange (getoperationvariable r i o))
				))) 
			(defaultrange (getvar r finishoutput)) returnpoints)));
		addclocked r (getvar r f.vdesc.varname) 
		(List.fold_left (fun a (i,_) -> 
			TERNARY (
				defaultrange (getcontrolvariable r i followcontrol),
				defaultrange (getreturnvariable r i),
				a
			)) (defaultrange (getvar r f.vdesc.varname)) returnpoints)


let vil_to_vast_connections (r:vastmodule) (m:vmodule) = 
	match m.minputs with
	| [c] -> oneconnection r m c
	| _ -> manyconnections r m

let vil_to_vast (f:funmodule):vastmodule = 
	let startcontrol = vil_to_vast_variable NA {varname=startinput; vtype={width=1; isSigned=false}}
	in let startfollowcontrol = vil_to_vast_variable REG {varname=startfollow; vtype={width=1; isSigned=false}}
	in let readycontrol = vil_to_vast_variable REG {varname=finishoutput; vtype={width=1; isSigned=false}}
	in let resultoutput = vil_to_vast_variable REG f.vdesc
	in let ret = {
		modname = f.vdesc.varname;
		inputs = startcontrol :: (List.map (vil_to_vast_variable NA) f.vinputs);
		outputs = readycontrol :: resultoutput :: [];
		locals = startfollowcontrol :: [];
		always = [];
		clockedge = {var={variable=startfollowcontrol; range=None; }; 
			assign=VARIABLE {variable=startcontrol; range=None}; blocking=false; } :: [];
	}
	in  List.iter (vil_to_vast_module ret f.vdesc) f.vmodules;
		List.iter (vil_to_vast_connections ret) f.vmodules;
		returnconnections ret f;
		ret;;