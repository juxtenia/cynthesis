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
			in if ci.cstruct then Struct (te,cel) else Union (te,cel)
		| _ -> E.s (E.error "Illegal type %a.\n" d_type t)
and generatecompelement (f:fieldinfo) = 
	let ret = {
		ename = f.fname;
		etype = generatetype f.ftype;
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
		{ varname = d.vname; vtype = (generatetype t) }
	| _ -> E.s (E.error "Illegal type %a.\n" d_type d.vtype) 

(* generates a vvarinfo from a Cil varinfo *)
let generatevariable (v:varinfo) :vvarinfo = 
	{
		varname = v.vname;
		vtype = generatetype v.vtype;
	}

(* generates a vconstinfo from a Cil constant *)
let generateconstant (c:constant) :vconstinfo = match c with
	| CInt64 (i64,k,_) -> {
		value = big_int_of_string (Int64.to_string i64); 
		ctype = generatetype (TInt (k,[]))
	}
	(** TODO ADD EXTRA constant types**)
	| _ -> E.s (E.error "Illegal constant %a.\n" d_const c) 

(* generates a list of vvarinfo from a list of Cil varinfo *)
let generatevariables :varinfo list -> vvarinfo list = List.map generatevariable 

(* generates an operation (adds it to m) from a operation type *)
let generateoperation (m:vmodule) (ot:voperationtype) = 
	let ret = {oid = !dataid; operation = ot; ousecount = 0; oschedule=emptyschedule}
    in dataid:= !dataid + 1;
    	m.mdataFlowGraph <- ret::m.mdataFlowGraph;
    	ret;;

(* the default function for getting an operation for a variable *)
let defaultvariables (m:vmodule) (v:vvarinfo) :voperation = 
	generateoperation m (Variable (v))

(* generates appropriate operations in m for the expression.
 * variables are resolved using vars *)
let rec generatedataflow (vars:vvarinfo -> voperation) (m:vmodule) (e:exp) :voperation = 
	match e with
		| Const c -> generateoperation m (Constant (generateconstant c))
  		| Lval l -> (match l with
  			| (Var(v),NoOffset) -> vars (generatevariable v)
  			| _ -> E.s (E.error "Illegal lvalue %a\n" d_lval l)
  		)
  		| UnOp (u,e1,t) -> generateoperation m (
  			Unary (generateunop u, generatedataflow vars m e1, generatetype t))
  		| BinOp (b,e1,e2,t) -> generateoperation m (Binary (
  			generatebinop b, generatedataflow vars m e1, 
  			generatedataflow vars m e2, generatetype t))
  		| CastE (t,e1) -> generateoperation m (
  			Unary (Cast, generatedataflow vars m e1, generatetype t))
  		| _ -> E.s (E.error "Illegal expression %a.\n" d_exp e)   	

(* generates a result for a given variable *)
let generateresult (m:vmodule) (o:voperation) (v:vvarinfo) :voperation =
	let optype = Result (v,o) in generateoperation m optype;;

let generatereturn (m:vmodule) (o:voperation) :voperation = 
	let optype = ReturnValue o in generateoperation m optype;;

(* generates operations for a Cil exp *)
let generateexp (m:vmodule) (e:exp) :voperation = 
	generatedataflow (defaultvariables m) m e;;

(* generates operations from a list of Cil instr *)
let rec generateinstrlist (ass:voperation list) (vars:vvarinfo -> voperation) (m:vmodule) (il:instr list) =
	match il with
	| h::t -> (match h with
		| Set (lv,e,l) -> (match lv with
  			| (Var(v),NoOffset) -> 
  				let result = generatedataflow vars m e
  				in let var = generatevariable v
  				in let as1 = 
  					if List.exists  
  						(fun f -> match f.operation with
  							| Result (i,o) when i.varname = var.varname -> 
  								f.operation <- Result (i,result); true
  							| _ -> false
  						) ass
  					then ass
  					else (generateresult m result var) :: ass;
  				in generateinstrlist as1 (fun v -> if v.varname = var.varname then result else vars v) m t
  			| _ -> E.s (E.error "Illegal lvalue %a.\n" d_lval lv)
  		)
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

(* generates a module from a Cil stmt *)
let generatemodule (v:vvarinfo) (entryid:int) (s:stmt) :vmodule = 
	let ret = 
	{
		mid = s.sid;
		minputs = [];
		moutputs = [];
		mvars = [];
		mvarexports = [];
		mdataFlowGraph = [];
	} in begin 
		(match s.skind with
			| Instr (il) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			); generateinstrlist [] (defaultvariables ret) ret il
  			| Return (eo,l) -> (match eo with
  					| None -> ()
  					| Some e -> ignore (generatereturn ret (generateexp ret e))
  				);
  				(match s.succs with
  					| [] -> ret.moutputs <- returnconnection s.sid
  					| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  				)
  			| Goto (_,l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Break (l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Continue (l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| If (e,b1,b2,l) -> let result = generateexp ret e
  				in (match s.succs with
  				| fb::tb::[] -> ret.moutputs <- ifconnection s.sid result tb.sid fb.sid 0.5
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  				)
  			| Loop (_,l,_,_) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Block (_) -> (match s.succs with
  				| h::[] -> ret.moutputs <- oneconnection s.sid h.sid
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			)
  			| _ -> E.s (E.error "Illegal stmt %a.\n" d_stmt s) 
  		);
  		if(s.sid = entryid) then ret.minputs <- 
  		{connectfrom=None;connectto=Some entryid;requires=None;probability=1.0}::ret.minputs
  			else ();
  		ret
	end

(* removes all stmts with no preds, excluding the first statement *)
let extractminentry (ss:stmt list) :(int *stmt list) = 
	match ss with 
		| [] -> E.s (E.error "Empty function body")
		| h::t -> (h.sid, h::(List.filter (fun s -> (List.length s.preds) <> 0) t))

(* generates a top level module from a function definition *)
let generatefunmodule (f:fundec) :funmodule = dataid := 0; (* Reset op ids *)
	(* generate variable for function *)
	let vardesc = generatedesc f.svar
	in let (entryid,filteredstmts) = (extractminentry f.sallstmts)
	(* basic starting point, this needs optimisation 
	 * and annotation before it is useful*)
	in let ret = {
		vdesc = vardesc;
		vinputs = generatevariables f.sformals;
		vlocals = generatevariables f.slocals;
		vmodules = List.map (generatemodule vardesc entryid) filteredstmts;
	}
	in  (* run optimisation pass *)
		Viloptimiser.optimisefunmodule ret;
		(* return *)
		ret