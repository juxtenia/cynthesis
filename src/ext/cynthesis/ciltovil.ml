open Big_int
open Vil
open Cil

(* id for operations, reset each time a function is synthesised *)
let dataid = ref 0;;

(* gives whether a type is signed *)
let rec typesigned (t:typ) :bool = match t with
	| TInt(ik,_) -> isSigned ik
	| TEnum(ei,_) -> isSigned ei.ekind
	| TNamed(ti,_) -> typesigned ti.ttype
	| TVoid _ 
	| TComp(_,_) 
		-> false
	| _ -> E.s (E.error "Illegal type %a.\n" d_type t)

let generatetypeelement (t:typ) :vtypeelement = {
		width = bitsSizeOf t;
		isSigned = typesigned t;
	}

let formstruct (te:vtypeelement) (cel:vcompelement list) = 
	te.width <- List.fold_left (fun i ce ->
		ce.ebase <- i; 
		i + (gettypeelement ce.etype).width
	) 0 cel;
	Struct (te,cel)

let formunion (te:vtypeelement) (cel:vcompelement list) = 
	te.width <- List.fold_left (fun i ce ->
		max i (gettypeelement ce.etype).width
	) 0 cel;
	Union (te,cel)

(* generates a vtype from a Cil typ *)
let rec generatetype (t:typ) :vtype = 
	let te = generatetypeelement t
	in  match t with
		| TVoid _ 
		| TInt (_,_)
		| TEnum (_,_)
			-> Basic te
		| TNamed (t1,_) 
			-> generatetype t1.ttype
		| TComp (ci,_) -> let cel = 
				List.map generatecompelement (List.filter (fun f ->
					missingFieldName <> f.fname ) ci.cfields)
			in if ci.cstruct then formstruct te cel else formunion te cel
		| _ -> E.s (E.error "Illegal type %a.\n" d_type t)
and generatecompelement (f:fieldinfo) = 
	let ret = {
		ename = f.fname;
		etype = generatetype f.ftype;
		ebase = 0;
	}
	in (match f.fbitfield with
			| None -> ()
			| Some i -> (match ret.etype with
				| Basic b -> b.width <- i
				| _ -> E.s (E.error "Illegal bitfield type %a.\n" d_type f.ftype)
			)
		);
		ret 

(* generates a unop from a Cil unop *)
let generateunop (u:Cil.unop) :Vil.unop = match u with
	| Neg -> Neg
	| BNot -> BNot
	| LNot -> LNot

(* generates a binop from a Cil binop *)
let generatebinop (b:Cil.binop) :Vil.binop = match b with
	| PlusA -> PlusA
	| MinusA -> MinusA
	| Mult -> Mult
	| Div -> Div
	| Mod -> Mod
	| Shiftlt -> Shiftlt
	| Shiftrt -> Shiftrt
	| Lt -> Lt
	| Gt -> Gt
	| Le -> Le
	| Ge -> Ge
	| Eq -> Eq
	| Ne -> Ne
	| BAnd -> BAnd
	| BXor -> BXor
	| BOr -> BOr
	| _ -> E.s (E.error "Illegal operation %a.\n" d_binop b) 

(* generates a description for a function from the Cil varinfo *)
let generatedesc (d:varinfo) :vvarinfo =
	match d.vtype with
	| TFun(t,Some a, false, _) -> 
		{ varname = d.vname; vtype = (generatetype t); }
	| _ -> E.s (E.error "Illegal type %a.\n" d_type d.vtype) 

(* generates a vvarinfo from a Cil varinfo *)
let generatevariable (v:varinfo) :vvarinfo = 
	{
		varname = v.vname;
		vtype = generatetype v.vtype;
	}

(* generates a vconstinfo from a Cil constant *)
let rec generateconstant (c:constant) :vconstinfo = match c with
	| CInt64 (i64,k,_) -> {
		value = big_int_of_string (Int64.to_string i64); 
		ctype = generatetype (TInt (k,[]));
	}
	| CChr (c) -> generateconstant (charConstToInt c)
	(** TODO ADD EXTRA constant types**)
	| _ -> E.s (E.error "Illegal constant %a.\n" d_const c) 

(* generates a list of vvarinfo from a list of Cil varinfo *)
let generatevariables :varinfo list -> vvarinfo list = List.map generatevariable 

let makeoperation (ot:voperationtype) = 
	let id = !dataid 
	in 	dataid:= !dataid + 1;
		{oid = id; operation = ot; ousecount = 0; oschedule=emptyschedule}

let getvarrange (v:varinfo) (o:offset) (lv:lval) = 
	let rec driver b t o1 = match o1 with
		| NoOffset -> (b,(gettypeelement t).width)
		| Field (fi,o2) -> (match t with
			| Union (_,cel) 
			| Struct (_,cel) -> let ce = List.find (fun (ce1:vcompelement) -> ce1.ename = fi.fname) cel
				in driver (b+ce.ebase) ce.etype o2
			| Basic _ -> E.s (E.error "Type %s has no field %s" (string_of_vtype t) fi.fname))
		| _ -> E.s (E.error "Illegal lvalue %a\n" d_lval lv)
	in driver 0 (generatetype v.vtype) o

(* generates appropriate operations in m for the expression.
 * variables are resolved using vars *)
let rec makedataflow (e:exp) :(voperation list * voperationlink) = 
	let (acn,ret) = 
	match e with
		| Const c -> ([], Simple (makeoperation (Constant (generateconstant c))))
  		| Lval l -> (match l with
  			| (Var(v),NoOffset) -> ([], Simple (makeoperation (Variable (generatevariable v))))
  			| (Var(v),o) -> let (b,w) = getvarrange v o l
  				in ([], Compound [{
  					loperation=(makeoperation (Variable (generatevariable v)));
  					lbase=b;
  					lwidth=w; }])
  			| _ -> E.s (E.error "Illegal lvalue %a\n" d_lval l)
  		)
  		| UnOp (u,e1,t) -> let (ac1,r1) = makedataflow e1
  			in (ac1, Simple(makeoperation (Unary (generateunop u, r1, generatetype t))))
  		| BinOp (b,e1,e2,t) -> let (ac1,r1) = makedataflow e1
  			in let (ac2,r2) = makedataflow e2
  			in (List.rev_append ac1 ac2, Simple(makeoperation (Binary (generatebinop b, r1, r2, generatetype t))))
  		| CastE (t,e1) -> let (ac1,r1) = makedataflow e1
  			in (ac1,Simple (makeoperation (Unary (Cast, r1, generatetype t))))
  		| _ -> E.s (E.error "Illegal expression %a.\n" d_exp e)   
  	in 	(List.rev_append (getlinkchildren ret) acn,ret)

let makereturn (e:exp) :voperation list = 
	let (ops,ln) = makedataflow e
	in (makeoperation (ReturnValue ln)) :: ops

let overwritevariable (v:vvarinfo) (b:int) (w:int) (ol:voperationlink) =
	let op = makeoperation (Variable v)
	in let initial = if b = 0 then [] 
		else [{lbase=0;lwidth=b;loperation=op;}]
	in let terminal = if b + w = (gettypeelement v.vtype).width then [] 
		else [{lbase=b+w;lwidth=(gettypeelement v.vtype).width-(b+w);loperation=op}]
	in let biddle = match ol with
		| Simple o -> [{lbase=0;lwidth=w;loperation=o;}]
		| Compound cll -> getrange 0 w cll
	in Compound (initial @ biddle @ terminal)

let generateset (lv:lval) (e:exp) :voperation list =
	let (ops,reslink) = makedataflow e 
	in let resulttype = match lv with
		| (Var(v),NoOffset) -> let v1 = generatevariable v
			in Result (v1,0,(gettypeelement v1.vtype).width,reslink)
		| (Var(v),o) -> let v1 = generatevariable v
			in let (b,w) = getvarrange v o lv
			in Result (v1,0,(gettypeelement v1.vtype).width, overwritevariable v1 b w reslink)
		| _ -> E.s (E.error "Illegal lvalue %a.\n" d_lval lv)
	in let result = makeoperation resulttype
	in result :: ops

(* generates operations from a list of Cil instr *)
let rec makeinstrlist (m:vblock) (il:instr list) =
	match il with
	| h :: t -> (match h with
		| Set (lv,e,l) -> 
			m.bdataFlowGraph <- mergeoperations m.bdataFlowGraph (generateset lv e) m.boutputs;
			makeinstrlist m t
		| Call (_,_,_,l) -> E.s (E.error "At %a illegal instr %a.\n" d_loc l d_instr h) 
		| Asm (_,_,_,_,_,l) -> E.s (E.error "At %a illegal instr %a.\n" d_loc l d_instr h) 
	)
	| [] -> ()

let oneconnection f t = {connectfrom = Some f; connectto = Some t; 
  					requires = None; probability = 1.0} :: []

let returnconnection f = {connectfrom = Some f; connectto = None; 
  					requires = None; probability = 1.0} :: []

let ifconnection f s tt tf p = 
	{connectfrom = Some f; connectto = Some tt; 
		requires = Some (s,true); probability=p} ::
  	{connectfrom = Some f; connectto = Some tf; 
  		requires = Some (s,false); probability=1.0-.p} :: []

let getvblocktype (s:stmt):vblocktype = match s.skind with
	| Instr (_) -> Instr
	| Return (_,_) -> Return
	| Goto (_,_)
	| Break (_) 
	| Continue (_) -> Goto
	| If (_,_,_,_) -> If
	| Loop (_,_,_,_) -> Loop
	| Block (_) -> Block
	| _ -> E.s (E.error "Illegal stmt %a.\n" d_stmt s) 

(* generates a module from a Cil stmt *)
let generatemodule (v:vvarinfo) (entryid:int) (s:stmt) :vblock = 
	let ret = 
	{
		bid = s.sid;
		btype = getvblocktype s;
		binputs = [];
		boutputs = [];
		bvars = [];
		bvarexports = [];
		bdataFlowGraph = [];
	} in begin 
		(match s.skind with
			| Instr (il) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			); makeinstrlist ret il
  			| Return (eo,l) -> (match eo with
  					| None -> ()
  					| Some e -> ret.bdataFlowGraph <- makereturn e
  				);
  				(match s.succs with
  					| [] -> ret.boutputs <- returnconnection s.sid
  					| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  				)
  			| Goto (_,l) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Break (l) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Continue (l) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| If (e,b1,b2,l) -> let (ops,result) = makedataflow e
  				in  ret.bdataFlowGraph <- ops;
  					(match s.succs with
  						| fb::tb::[] -> ret.boutputs <- ifconnection s.sid result tb.sid fb.sid 0.5
  						| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  					)
  			| Loop (_,l,_,_) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Block (_) -> (match s.succs with
  				| h::[] -> ret.boutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			)
  			| _ -> E.s (E.error "Illegal stmt %a.\n" d_stmt s) 
  		);
  		if(s.sid = entryid) then ret.binputs <- 
  		{connectfrom=None;connectto=Some entryid;requires=None;probability=1.0}::ret.binputs
  			else ();
  		ret
	end

(* removes identifies the entry point *)
let extractminentry (ss:stmt list) :int = 
	match ss with 
		| [] -> E.s (E.error "Empty function body")
		| h::t -> h.sid

(* generates a top level module from a function definition *)
let generatefunmodule (f:fundec) :funmodule = 
	(* Reset op ids *)
	dataid := 0; 
	(* generate variable for function *)
	let vardesc = generatedesc f.svar
	(* identify entry point, and remove some unreachable code *)
	in let entryid = (extractminentry f.sallstmts)
	(* basic starting point, this needs optimisation 
	 * and annotation before it is useful*)
	in let ret = {
		vdesc = vardesc;
		vinputs = generatevariables f.sformals;
		vlocals = generatevariables f.slocals;
		vblocks = List.map (generatemodule vardesc entryid) f.sallstmts;
	}
	in  (* run optimisation pass *)
		Viloptimiser.optimisefunmodule ret;
		(* return *)
		ret