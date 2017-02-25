open Vil
open Vilscheduler
open Vast
module E = Errormsg

let defaultrange (v:vastvariable) = VARIABLE { variable = v; range=None; indexing=[]; }

let onewidetype l = {width=1; isSigned=false; logictype=l; arraytype=[]; }
let siztyfourwidetype l = {width=64; isSigned=false; logictype=l; arraytype=[]; }
let onewidevar l n = {name=n; resetto=SINGLE Big_int.zero_big_int; typ=onewidetype l; } 

let zeroconst = {value = Big_int.zero_big_int; cwidth=1;}

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
let dspmul = "mul"
let dspdiv = "div"
let dsprem = "rem"
let dspinput1 = "input1"
let dspinput2 = "input2"
let dspresult = "result"
let getdspvariablename (mid:int) (s:string) = Printf.sprintf "dsp_%d_%s" mid s;;
let lookupoutput = "output"
let lookupenable = "enable"
let getlookupvariablename (name:string) (mid:int) (s:string) = Printf.sprintf "lookup_%s_%d_%s" name mid s;;

let getinputvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getinputvariablename bid v)
let getinputfollowvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getinputfollowvariablename bid v)
let getoutputvariable (r:vastmodule) (bid:int) (v:vvarinfo) = getvar r (getoutputvariablename bid v)
let getcontrolstartvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid startcontrol)
let getcontrolendvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid endcontrol)
let getcontrolvariablei (r:vastmodule) (bid:int) (s:int) = getvar r (getcontrolvariablename bid (string_of_int s))
let getcontrolfollowvariable (r:vastmodule) (bid:int) = getvar r (getcontrolvariablename bid followcontrol)
let getoperationvariable (r:vastmodule) (bid:int) (o:voperation) = getvar r (getoperationvariablename bid o)
let getreturnvariable (r:vastmodule) (bid:int) = getvar r (getreturnvariablename bid)
let getdspresultvariable (r:vastmodule) (mid:int) = getvar r (getdspvariablename mid dspresult)
let getlookupresultvariable (r:vastmodule) (name:string) (mid:int) = getvar r (getlookupvariablename name mid lookupoutput)

let makelvalnorange (n:string) (t:vasttype) = {
		variable={
			name = n;
			resetto = SINGLE Big_int.zero_big_int;
			typ = t;
		};
		range=None;
		indexing=[];
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
	in let controlstartalias = makelvalnorange (getcontrolvariablename m.bid (string_of_int 0))
		(onewidetype WIRE)
	in let controlendvariable = makelvalnorange (getcontrolvariablename m.bid endcontrol) 
		(onewidetype WIRE)
	in let controlfollowvariable = makelvalnorange (getcontrolvariablename m.bid followcontrol)
		(onewidetype REG)
	in  addvar r controlstartvariable;
		addwirevar r controlstartalias true (VARIABLE controlstartvariable);
		addwirevar r controlendvariable true (VARIABLE controlstartvariable);
		addregvar r controlfollowvariable (match l with
			| WIRE -> true
			| REG -> false
			| NA -> E.s (E.error "Variable %s can't have NA logic type" 
				controlstartvariable.variable.name)
		) (VARIABLE controlendvariable)

let makecontrolvariablesequence (i:int) (r:vastmodule) (m:vblock) = 
	let directconnect = List.length m.boutputs = 1
	in let controlstartvariable = makelvalnorange (getcontrolvariablename m.bid startcontrol) 
		(onewidetype REG)
	in let controlstartalias = makelvalnorange (getcontrolvariablename m.bid (string_of_int 0))
		(onewidetype WIRE)	
	in let controlendvariable = makelvalnorange (getcontrolvariablename m.bid endcontrol) 
		(onewidetype (if directconnect then WIRE else REG))
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
		addwirevar r controlstartalias true (VARIABLE controlstartvariable);
		addwirevar r controlendvariable directconnect (VARIABLE lastreg);
		addregvar r controlfollowvariable false (VARIABLE controlendvariable)

let makedspblock (r:vastmodule) (mid:int) =
	let input1 = makelvalnorange (getdspvariablename mid dspinput1) 
		(siztyfourwidetype WIRE)
	in let input2 = makelvalnorange (getdspvariablename mid dspinput2) 
		(siztyfourwidetype WIRE)
	in let result = makelvalnorange (getdspvariablename mid dspresult) 
		(siztyfourwidetype REG)
	in let mul = makelvalnorange (getdspvariablename mid dspmul) 
		(siztyfourwidetype WIRE)
	in let div = makelvalnorange (getdspvariablename mid dspdiv)
		(siztyfourwidetype WIRE)
	in let rem = makelvalnorange (getdspvariablename mid dsprem) 
		(siztyfourwidetype WIRE)
	in  addvar r input1;
		addvar r input2;
		addvar r mul;
		addvar r div;
		addvar r rem;
		addregvar r result false (
			TERNARY (VARIABLE mul, BINARY (MULT, VARIABLE input1, VARIABLE input2), 
			TERNARY (VARIABLE div, BINARY (DIV, VARIABLE input1, VARIABLE input2), 
			TERNARY (VARIABLE rem, BINARY (MOD, VARIABLE input1, VARIABLE input2), 
			VARIABLE result
		))))

let rec makedsps (r:vastmodule) (n:int) = if n = 0 then ()
	else (makedspblock r (n-1); makedsps r (n-1))

let rec makearraytype (i:vinitinfo) = match i with
	| Const _ -> []
	| Comp _ -> []
	| Array [] -> E.s (E.error "Invalid initialiser\n")
	| Array (h::t) -> (1 + List.length t) :: (makearraytype h)

let witharray (v:vasttype) (at:int list) = 
	{width = v.width; isSigned = v.isSigned; logictype = v.logictype; arraytype = at;}

let rec vil_to_vast_initialiser (i:vinitinfo) = match i with
	| Const c -> SINGLE c.value
	| Comp _ 
	| Array [] -> E.s (E.error "Invalid initialiser\n")
	| Array l -> ARRAY (List.map vil_to_vast_initialiser l)

let makelookupblock (r:vastmodule) (mid:int) (l:vlookupinfo) = 
	let viltype = baseinittype l.initialiser
	in let arraytype = makearraytype l.initialiser
	in let returntype = vil_to_vast_type REG viltype
	in let lookuptype = witharray returntype arraytype
	in let lookup = {
		variable={
			name = l.lookupname;
			resetto = vil_to_vast_initialiser l.initialiser;
			typ = lookuptype;
		};
		range=None;
		indexing=[]; 
	}
	in let output = makelvalnorange (getlookupvariablename l.lookupname mid lookupoutput) 
		returntype
	in let enable = makelvalnorange (getlookupvariablename l.lookupname mid lookupenable)
		(onewidetype WIRE)
	in let inputs = List.mapi (fun i a -> 
			makelvalnorange (getlookupvariablename l.lookupname mid (string_of_int i)) 
				{width=a;isSigned=false;logictype=WIRE;arraytype=[]}
		) arraytype
	in  List.iter (fun l -> addvar r l) inputs;
		addregvar r output false (TERNARY (VARIABLE enable,VARIABLE {
			variable=lookup.variable; 
			range=None;
			indexing=List.map (fun l -> VARIABLE l) inputs;
		},VARIABLE output))

let makelookup (r:vastmodule) (l:vlookupinfo) =
	let rec driver i = 
		if i = 0 then ()
		else (makelookupblock r (i-1) l; driver (i-1))
	in driver l.parrallelcount

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
	VARIABLE {
		variable=getoperationvariable r bid cl.loperation; 
		range=Some(cl.lbase+cl.lwidth-1,cl.lbase);
		indexing=[];
	}

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

let makeanoperation (r:vastmodule) (v:vvarinfo) (m:vblock) (o:voperation) = match getclass o with
	| DSP -> makeoperationregvariable r v m o (defaultrange (getdspresultvariable r o.oschedule.assigned))
	| Lookup s -> makeoperationregvariable r v m o (defaultrange (getlookupresultvariable r s o.oschedule.assigned))
	| Notcounted -> makeoperation r v m o

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
	maxtime m.bdataFlowGraph = 0 && List.length m.boutputs < 2

let zero_time_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	List.iter (makeinputvariable r m) m.bvars;
	makecontrolvariable WIRE r m;
	makeoperations makeoperationwire r v m [] [] m.bdataFlowGraph;
	let remainingoutputvars = List.filter (fun v -> not (hasvariableresult v m.bdataFlowGraph)) m.bvarexports
	in  List.iter (fun v -> 
			makeoutputwirevariable r m v (
				defaultrange (getinputvariable r m.bid v)
			)) remainingoutputvars	

let positive_time_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	List.iter (makeinputvariable r m) m.bvars;
	makecontrolvariablesequence (maxtime m.bdataFlowGraph) r m;
	makeoperations makeanoperation r v m [] [] m.bdataFlowGraph;
	let remainingoutputvars = List.filter (fun v -> not (hasvariableresult v m.bdataFlowGraph)) m.bvarexports
	in  List.iter (fun v -> 
			makeoutputwirevariable r m v (
				defaultrange (getinputvariable r m.bid v)
			)) remainingoutputvars	

let vil_to_vast_module (r:vastmodule) (v:vvarinfo) (m:vblock) =
	if iszerotime m
	then zero_time_module r v m
	else positive_time_module r v m
	
let addclocked (r:vastmodule) (v:vastvariable) (e:vastexpression) = 
	r.clockedge <- { var={ variable=v; range=None; indexing=[];};
		assign=e; blocking=false; } :: r.clockedge

let addalways (r:vastmodule) (v:vastvariable) (e:vastexpression) = 
	r.always <- { var={ variable=v; range=None; indexing=[];};
		assign=e; blocking=true; } :: r.always

let dspconnection (r:vastmodule) (f:funmodule) (n:int) = 
	let (mul_e,div_e,mod_e) = List.fold_left (fun e b ->
		List.fold_left (fun (emu,ed,emo) o -> if o.oschedule.assigned = n
			then match o.operation with
				| Binary(Mult,o1,o2,_) when not (allconst (getlinkchildren o1)) &&
					not (allconst (getlinkchildren o2))
				-> (BINARY(LOR,defaultrange (getcontrolvariablei r b.bid (o.oschedule.set -1)),emu),ed,emo)
				| Binary(Div,o1,o2,_) when not (allconst (getlinkchildren o1)) &&
					not (allconst (getlinkchildren o2))
				-> (emu,BINARY(LOR,defaultrange (getcontrolvariablei r b.bid (o.oschedule.set -1)),ed),emo)
				| Binary(Mod,o1,o2,_) when not (allconst (getlinkchildren o1)) &&
					not (allconst (getlinkchildren o2))
				-> (emu,ed,BINARY(LOR,defaultrange (getcontrolvariablei r b.bid (o.oschedule.set -1)),emo))
				| _ -> (emu,ed,emo)
			else (emu,ed,emo)
		) e b.bdataFlowGraph ) 
		(CONST zeroconst, CONST zeroconst, CONST zeroconst) 
		f.vblocks
	in  addalways r (getvar r (getdspvariablename n dspmul)) mul_e;
		addalways r (getvar r (getdspvariablename n dspdiv)) div_e;
		addalways r (getvar r (getdspvariablename n dsprem)) mod_e;
	let (input1_e,input2_e) = List.fold_left (fun e b -> 
		List.fold_left (fun (e1,e2) o -> if o.oschedule.assigned = n
			then match (getclass o,o.operation) with
				| (DSP,Binary(_,o1,o2,_)) -> 
					let cvar = defaultrange (getcontrolvariablei r b.bid (o.oschedule.set -1))
					in (TERNARY (cvar,refertooperation r b.bid o1,e1),
						TERNARY (cvar,refertooperation r b.bid o2,e2))
				| _ -> (e1,e2)
			else (e1,e2)
		) e b.bdataFlowGraph ) 
		(CONST zeroconst, CONST zeroconst) 
		f.vblocks
	in  addalways r (getvar r (getdspvariablename n dspinput1)) input1_e;
		addalways r (getvar r (getdspvariablename n dspinput2)) input2_e

let rec dspconnections (r:vastmodule) (f:funmodule) (n:int) = if n = 0 then ()
	else (dspconnection r f (n-1); dspconnections r f (n-1))

let rec lookupconnection (r:vastmodule) (f:funmodule) (l:vlookupinfo) (n:int) =
	let width = List.length (makearraytype l.initialiser)
	in let (input_e_list,enable_e) = List.fold_left (fun e b -> 
		List.fold_left (fun (e1l,e2) o -> if o.oschedule.assigned = n
			then match (getclass o,o.operation) with
				| (Lookup _,Lookup(s,oll,_)) when s = l.lookupname
				-> let cvar = defaultrange (getcontrolvariablei r b.bid (o.oschedule.set -1))
					in (List.map2 (fun o1 e1 ->
						TERNARY(cvar,refertooperation r b.bid o1,e1)
						) oll e1l,
						BINARY (LOR,cvar,e2))
				| _ -> (e1l,e2)
			else (e1l,e2)
		) e b.bdataFlowGraph ) 
		(Listutil.ntimes (CONST zeroconst) width, CONST zeroconst) 
		f.vblocks
	in  List.iteri (fun i e -> 
			addalways r (getvar r (getlookupvariablename l.lookupname n (string_of_int i))) e
		) input_e_list;
		addalways r (getvar r (getlookupvariablename l.lookupname n lookupenable)) enable_e

let lookupconnections (r:vastmodule) (f:funmodule) (l:vlookupinfo) = 
	let rec driver n = 
		if n = 0 then ()
		else (lookupconnection r f l (n-1); driver (n-1))
	in driver l.parrallelcount

let startinput = "start"
let startfollow = "startfollow"
let finishoutput = "finish"

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
		clockedge = {var={variable=startfollowcontrol; range=None; indexing=[];}; 
			assign=defaultrange startcontrol; blocking=false; } :: [];
	}
	in  List.iter (makelookup ret) f.vglobals;
		makedsps ret !Vilscheduler.dspcount;
		List.iter (vil_to_vast_module ret f.vdesc) f.vblocks;
		List.iter (vil_to_vast_connections ret) f.vblocks;
		returnconnections ret f;
		List.iter (lookupconnections ret f) f.vglobals;
		dspconnections ret f !Vilscheduler.dspcount;
		ret;;