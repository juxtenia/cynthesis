open Big_int
open Cil
module E = Errormsg

(** Unary Operations*)
type unop =
  | Cast                                (** Cast expression*)
  | Neg                                 (** Unary minus *)
  | BNot                                (** Bitwise complement (~) *)
  | LNot                                (** Logical Not (!) *)
(** Binary operations *)
and binop =
  | PlusA                               (** arithmetic + *)
  | MinusA                              (** arithmetic - *)
  | Mult                                (** * *)
  | Div                                 (** / *)
  | Mod                                 (** % *)
  | Shiftlt                             (** shift left *)
  | Shiftrt                             (** shift right *)
  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)  
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)            
  | BAnd                                (** bitwise and *)
  | BXor                                (** bitwise exclusive-or *)
  | BOr                                 (** bitwise inclusive-or *)
and funmodule = {
	mutable vdesc: vvarinfo;
		(** The name and output type of the function *) 
	mutable vinputs: vvarinfo list;
		(** The parameters to the function *)
	mutable vlocals: vvarinfo list;
		(** The local variables in the function *)
	mutable vcontrolconnections: vconnection list;
		(** The connections to provide control flow *)
	mutable vmodules: vmodule list;
		(** The modules that provide internal functionality *)
}
and vvarinfo = {
	mutable varname: string;
		(** The name of the variable *)
	mutable vtype: vtype;
		(** the type of this constant *)
}
and vconstinfo = {
	mutable value: big_int;
		(** the value of this constant *)
	mutable ctype: vtype;
		(** the type of this constant *)
}
and vconnection = {
	mutable connectfrom: int option;
		(** The id of the exporting module, none means from the start *)
	mutable connectto: int option;
		(** The id of the importing module, none means computation ends *)
	mutable requires: (voperation * bool) option;
		(** Optional requirement for exporting, The value of the operation 
			with the given id must have the same c truth value as the bool 
			provided *)
}
and vmodule = {
	mutable mid: int;
		(** The id of this module in the function *)
	mutable minputs: vconnection list;
		(** The incoming connections *)
	mutable moutputs: vconnection list;
		(** The posible modules to hand control flow to *)
	mutable mdataFlowGraph: voperation list;
}
and voperation = {
	mutable oid: int;
		(** id used to remove duplicates inside a module *)
	mutable operation: voperationtype;
		(** the type of this operation*)
}
and voperationtype = 
	| Variable of vvarinfo
		(** The value of a variable as it enters the module *)
	| Constant of vconstinfo
		(** The value of a constant *)
	| Result of vvarinfo * voperation
		(** marks the value of a variable that should be passed to the next 
			stage *)
	| Unary of unop * voperation * vtype
		(** applies a unary operation to the previous item *)
	| Binary of binop * voperation * voperation * vtype
		(** applies a binary operation to the previous items *)  
and vtype = {
	mutable width: int;
		(** The width of the type, in bits *)
	mutable isSigned: bool;
		(** Whether this is a signed variable *)
}

let rec string_of_unop u = match u with
  | Neg -> "Neg"
  | BNot -> "BNot"
  | LNot -> "LNot"
  | Cast -> "Cast"
and string_of_binop b = match b with
  | PlusA  -> "PlusA "
  | MinusA -> "MinusA"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Shiftlt -> "Shiftlt"
  | Shiftrt -> "Shiftrt"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Le -> "Le"
  | Ge -> "Ge"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | BAnd -> "BAnd"
  | BXor -> "BXor"
  | BOr -> "BOr"
and string_of_funmodule f = 
	"{ vdesc:" ^ (string_of_vvarinfo f.vdesc)
	^ ", vinputs:[" ^ (String.concat ", " (List.map string_of_vvarinfo f.vinputs))
	^ "], vlocals:[" ^ (String.concat ", " (List.map string_of_vvarinfo f.vlocals))
	^ "], vcontrolconnections:[" ^ (String.concat ", " (List.map string_of_vconnection f.vcontrolconnections))
	^ "], vmodules:[" ^ (String.concat ", " (List.map string_of_vmodule f.vmodules))
	^ "]}"
and string_of_vvarinfo v = 
	"{ varname:\"" ^ v.varname
	^ "\", vtype:" ^ (string_of_vtype v.vtype)
	^ "}"
and string_of_vconstinfo c = 
	"{ value:" ^ (string_of_big_int c.value)
	^ ", vtype:" ^ (string_of_vtype c.ctype)
	^ "}"
and string_of_vconnection c = 
	"{ connectfrom:" ^ (match c.connectfrom with 
		| Some i -> (string_of_int i)
		| None -> "None"
	)
	^ ", connectto:" ^ (match c.connectto with 
		| Some i -> (string_of_int i)
		| None -> "None"
	)
	^ ", requires:" ^ (match c.requires with 
		| Some (o,b) -> "Some:(" ^ (string_of_int o.oid) ^ ", " ^ (string_of_bool b) ^ ")"
		| None -> "None"
	)
	^ "}"
and string_of_vmodule m = 
	"{ mid:" ^ (string_of_int m.mid)
	^ ", minputs:[" ^ (String.concat ", " (List.map string_of_vconnection m.minputs))
	^ "], moutputs:[" ^ (String.concat ", " (List.map string_of_vconnection m.moutputs))
	^ "], mdataFlowGraph:[" ^ (String.concat ", " (List.map string_of_voperation m.mdataFlowGraph))
	^ "]}"
and string_of_voperation o = 
	"{ oid:" ^ (string_of_int o.oid)
	^ ", operation:" ^ (string_of_voperationtype o.operation)
	^ "}"
and string_of_voperationtype vt = "{" ^ (match vt with
	| Variable v -> "Variable:" ^ (string_of_vvarinfo v)
	| Constant c -> "Constant:" ^ (string_of_vconstinfo c)
	| Result (v,o) -> "Result:(" ^ (string_of_vvarinfo v) ^ ", " ^ (string_of_int o.oid) ^ ")"
	| Unary (u,o,t) -> "Unary:(" ^ (string_of_unop u) ^ ", " ^ (string_of_int o.oid) ^ ", " 
		^ (string_of_vtype t) ^ ")" 
	| Binary (u,o1,o2,t) -> "Binary:(" ^ (string_of_binop u) ^ ", " ^ (string_of_int o1.oid) 
		^ ", " ^ (string_of_int o2.oid) ^ ", " ^ (string_of_vtype t) ^ ")" 
	) ^ "}"
and string_of_vtype t = 
	"{ width:" ^ (string_of_int t.width)
	^ ", isSigned:" ^ (string_of_bool t.isSigned)
	^ "}"

let dataid = ref 0;;

let blanktype :vtype = {width = 0; isSigned = false};;

let rec typesigned (t:typ) :bool = match t with
	| TInt(ik,_) -> isSigned ik
	| TNamed(ti,_) -> typesigned ti.ttype
	| TComp(_,_) -> false
	| TEnum(_,_) -> false
	| _ -> E.s (E.error "Illegal type %a.\n" d_type t)

let generatetype (t:typ) :vtype = 
	{
		width = bitsSizeOf t;
		isSigned = typesigned t;
	}

let generateunop (u:Cil.unop) :unop = match u with
	| Neg -> Neg
	| BNot -> BNot
	| LNot -> LNot

let generatebinop (b:Cil.binop) :binop = match b with
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

let generatedesc (d:varinfo) :vvarinfo =
	match d.vtype with
	| TFun(t,Some a, false, _) -> 
		{ varname = d.vname; vtype = (generatetype t) }
	| _ -> E.s (E.error "Illegal type %a.\n" d_type d.vtype) 

let generatevariable (v:varinfo) :vvarinfo = 
	{
		varname = v.vname;
		vtype = generatetype v.vtype;
	}

let generateconstant (c:constant) :vconstinfo = match c with
	| CInt64 (i64,k,_) -> {
		value = big_int_of_string (Int64.to_string i64); 
		ctype = generatetype (TInt (k,[]))
	}
	(** TODO ADD EXTRA constant types**)
	| _ -> E.s (E.error "Illegal constant %a.\n" d_const c) 


let generatevariables (vs:varinfo list) :vvarinfo list =
	List.map generatevariable vs

let generateoperation (m:vmodule) (ot:voperationtype) = 
	let ret = {oid = !dataid; operation = ot}
    in dataid:= !dataid + 1;
    	m.mdataFlowGraph <- ret::m.mdataFlowGraph;
    	ret;;

let defaultvariables (m:vmodule) (v:vvarinfo) :voperation = 
	generateoperation m (Variable (v))

let rec generatedataflow (vars:vvarinfo -> voperation) (m:vmodule) (e:exp) :voperation = 
	match e with
		| Const c -> generateoperation m (Constant (generateconstant c))
  		| Lval l -> (match l with
  			| (Var(v),NoOffset) -> vars (generatevariable v)
  			| _ -> E.s (E.error "Illegal lvalue %a.\n" d_lval l)
  		)
  		| UnOp (u,e1,t) -> generateoperation m (
  			Unary (generateunop u, generatedataflow vars m e1, generatetype t))
  		| BinOp (b,e1,e2,t) -> generateoperation m (Binary (
  			generatebinop b, generatedataflow vars m e1, 
  			generatedataflow vars m e2, generatetype t))
  		| CastE (t,e1) -> generateoperation m (
  			Unary (Cast, generatedataflow vars m e1, generatetype t))
  		| _ -> E.s (E.error "Illegal expression %a.\n" d_exp e)   	

let generateresult (m:vmodule) (o:voperation) (v:vvarinfo) :voperation =
	let optype = Result (v,o) in generateoperation m optype;;

let generateexp (m:vmodule) (e:exp) :voperation = 
	generatedataflow (defaultvariables m) m e;;

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
  				in generateinstrlist as1 
  					(fun v -> if v.varname = var.varname then result else vars v)
  					m t
  			| _ -> E.s (E.error "Illegal lvalue %a.\n" d_lval lv)
  		)
		| Call (_,_,_,l) -> E.s (E.error "At %a illegal instr %a.\n" d_loc l d_instr h) 
		| Asm (_,_,_,_,_,l) -> E.s (E.error "At %a illegal instr %a.\n" d_loc l d_instr h) 
	)
	| [] -> ()

let generatemodule (v:vvarinfo) (s:stmt) :vmodule = 
	let ret = 
	{
		mid = s.sid;
		minputs = [];
		moutputs = [];
		mdataFlowGraph = [];
	} in begin 
		(match s.skind with
			| Instr (il) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			); generateinstrlist [] (defaultvariables ret) ret il
  			| Return (eo,l) -> (match eo with
  					| None -> ()
  					| Some e -> ignore (generateresult ret (generateexp ret e) v)
  				);
  				(match s.succs with
  					| [] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = None; requires = None} :: []
  					| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  				)
  			| Goto (_,l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Break (l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Continue (l) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| If (e,b1,b2,l) -> let result = generateexp ret e
  				in (match s.succs with
  				| tb::fb::[] -> ret.moutputs <- 
  					{connectfrom = Some s.sid; connectto = Some tb.sid; requires = Some (result,true)} ::
  					{connectfrom = Some s.sid; connectto = Some fb.sid; requires = Some (result,false)} :: []
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  				)
  			| Loop (_,l,_,_) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "At %a stmt %a has incorrect successors\n" d_loc l d_stmt s)
  			)
  			| Block (_) -> (match s.succs with
  				| h::[] -> ret.moutputs <- {connectfrom = Some s.sid; connectto = Some h.sid; requires = None} :: []
  				| _ -> E.s (E.error "Stmt %a has incorrect successors\n" d_stmt s)
  			)
  			| _ -> E.s (E.error "Illegal stmt %a.\n" d_stmt s) 
  		);
  		ret
	end

let replaceone (reps :(voperation*voperation) list) (op: voperation) = 
	try snd (List.find (fun (f,_) -> f.oid = op.oid) reps)
		with | Not_found -> op

let replaceoperations (reps :(voperation*voperation) list) (ops :voperation list) = List.map 
	(fun o -> let op = 
		match o.operation with
			| Result (v,o1) -> Result(v,replaceone reps o1)
			| Unary (u,o1,t) -> Unary(u,replaceone reps o1,t)
			| Binary (b,o1,o2,t) -> Binary(b,replaceone reps o1, replaceone reps o2,t)
			| _ -> o.operation
		in {oid=o.oid; operation = op}
	) ops

let mergemodules (m1:vmodule) (m2:vmodule) = 
	let ret = {
			mid = m1.mid;
			minputs = m1.minputs;
			moutputs = m2.moutputs;
			mdataFlowGraph = [];
		}
	in let first_half = List.filter 
		(fun o -> match o.operation with
			| Result(i,_) when List.exists 
				(fun o1 -> match o1.operation with
					| Result(i1,_) when i1.varname = i.varname -> true
					| _ -> false
				) m2.mdataFlowGraph -> false
			| _ -> true
		) m1.mdataFlowGraph
	in let replacements = ref []
	in let second_half = List.filter
		(fun o -> match o.operation with
			| Variable i when List.exists 
				(fun o1 -> match o1.operation with
					| Result(i1,o2) when i1.varname = i.varname -> 
						replacements := (o,o2) :: !replacements;
						true
					| _ -> false
				) m1.mdataFlowGraph -> false
			| _ -> true
		) m2.mdataFlowGraph
	in ret.mdataFlowGraph <- List.rev_append first_half 
			(replaceoperations !replacements second_half);
		List.iter (fun c -> c.connectfrom <- Some ret.mid) ret.moutputs;
		List.iter (fun c -> c.connectto <- Some ret.mid) ret.minputs;
		ret;; 

let rec compactmodules (acc:vmodule list) (mods:vmodule list) =
	match mods with
	| [] -> acc
	| h::t -> (match h.moutputs with
		| [{connectfrom = Some cfrom; connectto = Some cto; requires = None}] ->
			let filt = (fun m -> m.mid = cto)
			in let (it,rem,ac1) = if List.exists filt acc
				then let (it1,ac2) = List.partition filt acc in (it1,t,ac2)
				else let (it1,rem1) = List.partition filt t in (it1,rem1,acc)
			in (match it with
				| [m] -> if (List.length m.minputs = 1)
					then compactmodules ac1 ((mergemodules h m) :: rem)
					else compactmodules (h :: acc) t
				| _ -> E.s (E.error "Incorrect module connections or ids: [%s]" (String.concat ", " (List.map string_of_vmodule it)))
			)
		| _ -> compactmodules (h :: acc) t
	)

let generateconnections (m:funmodule) = List.iter 
	(fun m1 -> List.iter 
		(fun c ->
			match c.connectto with
				| Some i -> List.iter 
					(fun m2 -> if m2.mid = i 
						then (m.vcontrolconnections <- (c:: m.vcontrolconnections);
							 m2.minputs <- (c :: m2.minputs))
						else ()
					) m.vmodules
				| None -> ()
		) m1.moutputs
	) m.vmodules;
	let count = ref 0
	in List.iter (fun m1 -> if List.length m1.minputs = 0 
		then (count := !count + 1;
			m.vcontrolconnections <- 
				{connectfrom = None; connectto = Some m1.mid; requires = None} :: m.vcontrolconnections;
			m1.minputs <- [{connectfrom = None; connectto = Some m1.mid; requires = None}])
		else ()
	) m.vmodules;
	if not (!count = 1) 
		then E.s (E.error "%d != 1 entry points to function %s" !count m.vdesc.varname)
	;;

let funtofunmodule (f:fundec) :funmodule = let _ = dataid := 0; 
	in let vardesc = generatedesc f.svar
	in let ret = {
		vdesc = vardesc;
		vinputs = generatevariables f.sformals;
		vlocals = generatevariables f.slocals;
		vcontrolconnections = [];
		vmodules = List.map (generatemodule vardesc) f.sallstmts;
	} in begin
		generateconnections ret; 
		ret.vmodules <- compactmodules [] ret.vmodules;
		List.iter (fun m -> m.minputs <- []) ret.vmodules;
		ret.vcontrolconnections <- [];
		generateconnections ret; 
		ret
	end

