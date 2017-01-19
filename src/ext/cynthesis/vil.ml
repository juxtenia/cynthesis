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
	mutable ousecount: int;
		(** the number of users of this operation *)
	mutable oschedule: vscheduleinfo;
		(** the scheduling data of this operation *)
}
and vscheduleinfo = {
	earliest: int;
		(** asap schedule *)
	latest: int;
		(** alap schedule*)
	set: int;
		(** actual schedule *)
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

let emptyschedule = {earliest = 0; latest = 0; set = 0};;

(** The following functions produce a JSON like dump of all the 
 *  information inside any of the above data types. Since there are
 *  typically multiple references to certain items within modules,
 *  some items are represented only by an id.
 *)
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
	^ ", ousecount:" ^ (string_of_int o.ousecount) 
	^ ", oschedule:" ^ (string_of_vscheduleinfo o.oschedule)
	^ ", operation:" ^ (string_of_voperationtype o.operation)
	^ "}"
and string_of_vscheduleinfo si = 
	"{ earliest:" ^ (string_of_int si.earliest)
	^ ", latest:" ^ (string_of_int si.latest) 
	^ ", set:" ^ (string_of_int si.set)
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
(*  The following functions attempt to give a quick readable
 *  output from the various types, intented for console output
 *)
let rec print_unop u = match u with
  | Neg -> "-"
  | BNot -> "~"
  | LNot -> "!"
  | Cast -> "<cast>"
and print_binop b = match b with
  | PlusA  -> "+"
  | MinusA -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Shiftlt -> "<<"
  | Shiftrt -> ">>"
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | BAnd -> "&"
  | BXor -> "^"
  | BOr -> "|"
and print_funmodule f = 
	(print_vvarinfo f.vdesc)
	^ "\n\tinputs:[" ^ (String.concat ", " (List.map print_vvarinfo f.vinputs))
	^ "]\n\tlocals:[" ^ (String.concat ", " (List.map print_vvarinfo f.vlocals))
	^ "]\n\tmodules:\n\t" ^ (String.concat "\n\t" (List.map print_vmodule f.vmodules))
and print_vvarinfo v = 
	v.varname ^ ": " ^ (print_vtype v.vtype)
and print_vconstinfo c = 
	(string_of_big_int c.value) ^ ": " ^ (print_vtype c.ctype)
and print_vconnections (c:vconnection list) = match c with
	| [{connectfrom=f;connectto=t;requires=None}] -> "-> " ^ (match t with 
		| Some i -> (string_of_int i)
		| None -> "return"
	)
	| [{connectfrom=f1;connectto=tt;requires=Some(o1,true)};{connectfrom=f2;connectto=tf;requires=Some(o2,false)}] when f1 = f2 || o1 = o2
		-> "-> " ^ (string_of_int o1.oid) ^ " ? " ^ (match tt with 
		| Some i -> (string_of_int i)
		| None -> "return"
	) ^ " : " ^ (match tf with 
		| Some i -> (string_of_int i)
		| None -> "return"
	)
	| _ -> "[" ^ (String.concat ", " (List.map string_of_vconnection c)) ^ "]"
and print_vmodule m = 
	(string_of_int m.mid) ^ " " ^ (print_vconnections m.moutputs)
	^ "\n\t\t" ^ (String.concat "\n\t\t" (List.map print_voperation m.mdataFlowGraph))
and print_voperation o = 
	(string_of_int o.oid) ^ " ==== " ^ (print_voperationtype o.operation)
and print_voperationtype vt = match vt with
	| Variable v -> print_vvarinfo v
	| Constant c -> print_vconstinfo c
	| Result (v,o) -> v.varname ^ " = <" ^ (string_of_int o.oid) ^">"
	| Unary (u,o,t) -> (print_unop u) ^ "<" ^ (string_of_int o.oid) ^ ">: " 
		^ (print_vtype t) 
	| Binary (u,o1,o2,t) -> "<" ^ (string_of_int o1.oid) ^ "> " ^ (print_binop u) ^ " <" ^ 
		(string_of_int o2.oid) ^ ">: " ^ (print_vtype t)
and print_vtype t = 
	(string_of_int t.width) ^ "'" ^ if t.isSigned then "s" else "u"

(* id for operations, reset each time a function is synthesised *)
let dataid = ref 0;;

(* gives whether a type is signed *)
let rec typesigned (t:typ) :bool = match t with
	| TInt(ik,_) -> isSigned ik
	| TNamed(ti,_) -> typesigned ti.ttype
	| TComp(_,_) -> false
	| TEnum(_,_) -> false
	| _ -> E.s (E.error "Illegal type %a.\n" d_type t)

(* generates a vtype from a Cil typ *)
let generatetype (t:typ) :vtype = 
	{
		width = bitsSizeOf t;
		isSigned = typesigned t;
	}

(* generates a unop from a Cil unop *)
let generateunop (u:Cil.unop) :unop = match u with
	| Neg -> Neg
	| BNot -> BNot
	| LNot -> LNot

(* generates a binop from a Cil binop *)
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
let generatevariables (vs:varinfo list) :vvarinfo list =
	List.map generatevariable vs

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

(* generates a result for a given variable *)
let generateresult (m:vmodule) (o:voperation) (v:vvarinfo) :voperation =
	let optype = Result (v,o) in generateoperation m optype;;

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

(* generates a module from a Cil stmt *)
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
  				| fb::tb::[] -> ret.moutputs <- 
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

(* gives if two types are equal *)
let eq_type (t1:vtype) (t2:vtype) = 
	t1.width = t2.width && t1.isSigned = t2.isSigned

(* gives if two unops are equal *)
let eq_unop (u1:unop) (u2:unop) = match (u1,u2) with
	| (Neg,Neg) -> true
	| (BNot,BNot) -> true
	| (LNot,LNot) -> true
	| _ -> false

(* gives if two binops are equal *)
let eq_binop (b1:binop) (b2:binop) = match (b1,b2) with
	| (PlusA,PlusA) -> true
	| (MinusA,MinusA) -> true
	| (Mult,Mult) -> true
	| (Div,Div) -> true
	| (Mod,Mod) -> true
	| (Shiftlt,Shiftlt) -> true
	| (Shiftrt,Shiftrt) -> true
	| (Lt,Lt) -> true
	| (Gt,Gt) -> true
	| (Le,Le) -> true
	| (Ge,Ge) -> true
	| (Eq,Eq) -> true
	| (Ne,Ne) -> true
	| (BAnd,BAnd) -> true
	| (BXor,BXor) -> true
	| (BOr,BOr) -> true
	| _ -> false

(* gives if two operationtypes are equal *)
let eq_operation_type (ot1:voperationtype) (ot2:voperationtype) = 
	match (ot1,ot2) with
	| (Variable v1, Variable v2) when v1.varname = v2.varname -> true
	| (Constant c1, Constant c2) when (eq_big_int c1.value c2.value && eq_type c1.ctype c2.ctype) -> true
	| (Result (v1, o1), Result (v2, o2)) when v1.varname = v2.varname && o1.oid = o2.oid -> true
	| (Unary (u1,o1,t1), Unary (u2,o2,t2)) when eq_unop u1 u2 && o1.oid = o2.oid && eq_type t1 t2 -> true
	| (Binary (b1,o11,o21,t1), Binary(b2,o12,o22,t2)) when eq_binop b1 b2 && o11.oid = o12.oid && o21.oid = o22.oid && eq_type t1 t2 -> true
	| _ -> false

(* gives if two operations are equal *)
let eq_operation (o1:voperation) (o2:voperation) = 
	o1.oid = o2.oid && eq_operation_type o1.operation o2.operation

(* gives the result of running all replacements in reps on the operation op *)
let replaceone (reps :(voperation*voperation) list) (op: voperation) = 
	try snd (List.find (fun (f,_) -> f.oid = op.oid) reps)
	with | Not_found -> op

(* replace the targeted operations in condition requirements *)
let replaceconditions (reps :(voperation*voperation) list) (cs:vconnection list) = List.iter
	(fun c -> let req = 
		match c.requires with
			| None -> None
			| Some (o,b) -> Some (replaceone reps o,b)
		in c.requires <- req
	) cs

(* replace operations in the sub trees of operations in the list *)
let replaceoperations (reps :(voperation*voperation) list) (ops :voperation list) = List.iter 
	(fun o -> let op = 
		match o.operation with
			| Result (v,o1) -> Result(v,replaceone reps o1)
			| Unary (u,o1,t) -> Unary(u,replaceone reps o1,t)
			| Binary (b,o1,o2,t) -> Binary(b,replaceone reps o1, replaceone reps o2,t)
			| _ -> o.operation
		in o.operation <- op
	) ops

(* merge two modules together *)
let mergemodules (m1:vmodule) (m2:vmodule) = 
	let ret = {
			mid = m1.mid;
			minputs = m1.minputs;
			moutputs = m2.moutputs;
			mdataFlowGraph = [];
		}
	(* remove result tags if one exists in second module *)
	in let first_half = List.filter 
		(fun o -> match o.operation with
			| Result(i,_) when List.exists 
				(fun o1 -> match o1.operation with
					| Result(i1,_) when i1.varname = i.varname -> true
					| _ -> false
				) m2.mdataFlowGraph -> false
			| _ -> true
		) m1.mdataFlowGraph
	(* things to replace, variable references and what they 
	 * were set to in the first module *)
	in let replacements = ref []
	(* remove variable accesses that were set in first module and add replacements *)
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
	in  (* do replacements *)
		replaceoperations !replacements second_half;
		(* join the two together (rev_append is tail recursive) *)
		ret.mdataFlowGraph <- List.rev_append first_half second_half;
		(* update the id's of the connections *)
		List.iter (fun c -> c.connectfrom <- Some ret.mid) ret.moutputs;
		(* do the same replacements on the condiditons *)
		replaceconditions !replacements ret.moutputs;
		(* return *)
		ret;; 

(* merge modules with redundant control flow *)
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
					(* add merged module to head of list in case of further merges *)
					then compactmodules ac1 ((mergemodules h m) :: rem)
					(* skip module *)
					else compactmodules (h :: acc) t
				| _ -> E.s (E.error "Incorrect module connections or ids: [%s]" (String.concat ", " (List.map string_of_vmodule it)))
			)
		| _ -> compactmodules (h :: acc) t
	)

(* generate minputs for modules *)
let generateconnections (m:funmodule) = List.iter 
	(fun m1 -> List.iter 
		(fun c ->
			match c.connectto with
				| Some i -> List.iter 
					(fun m2 -> if m2.mid = i 
						then m2.minputs <- (c :: m2.minputs)
						else ()
					) m.vmodules
				| None -> ()
		) m1.moutputs
	) m.vmodules;
	let count = ref 0
	in List.iter (fun m1 -> if List.length m1.minputs = 0 
		then (count := !count + 1; 
			m1.minputs <- [{connectfrom = None; connectto = Some m1.mid; requires = None}])
		else ()
	) m.vmodules;
	if not (!count = 1) 
		then E.s (E.error "%d <> 1 entry points to function %s" !count m.vdesc.varname)
	;;

(* remove unused variables (caused by merging modules) *)
let variablecull (f:funmodule) = 
	(* remove unused locals all together *)
	f.vlocals <- List.filter
	(fun v -> 
		let ret = List.exists 
			(fun m ->
				List.exists
				(fun o -> 
					match o.operation with
						| Variable v1 when v1.varname = v.varname -> true
						| _ -> false
				)
				m.mdataFlowGraph
			) f.vmodules
		in
			if not ret 
			then List.iter 
				(fun m -> 
					m.mdataFlowGraph <- List.filter (fun o -> 
						match o.operation with
							| Result (v1,_) when v1.varname = v.varname -> false
							| _ -> true
					) m.mdataFlowGraph
				) f.vmodules;
			ret
	)
	f.vlocals;
	(* can't remove function arguments for compatability, but can remove assignments *)
	List.iter
	(fun v -> 
		let ret = List.exists 
			(fun m ->
				List.exists
				(fun o -> 
					match o.operation with
						| Variable v1 when v1.varname = v.varname -> true
						| _ -> false
				)
				m.mdataFlowGraph
			) f.vmodules
		in
			if not ret 
			then List.iter 
				(fun m -> 
					m.mdataFlowGraph <- List.filter (fun o -> 
						match o.operation with
							| Result (v1,_) when v1.varname = v.varname -> false
							| _ -> true
					) m.mdataFlowGraph
				) f.vmodules
			else ()
	)
	f.vinputs

(* increment operation use count *)
let incoperationcount (op:voperation) = 
	op.ousecount <- op.ousecount + 1

(* decrement operation use count *)
let decoperationcount (op:voperation) = 
	op.ousecount <- op.ousecount - 1

(* applies f to the immediate children of op *)
let dotoimmediatechildren (f:voperation -> unit) (op:voperation) = 
	match op.operation with
	| Unary(_,o,_) -> f o
	| Binary(_,o1,o2,_) -> f o1; f o2
	| Result(_,o) -> f o
	| _ -> ()

(* simple uses of dotoimmedaitechildren for tracking use counts *)
let incchildren = dotoimmediatechildren incoperationcount;;
let decchildren = dotoimmediatechildren decoperationcount;;

(* build operation counts *)
let rec dooperationcounts (cs:vconnection list) (ops:voperation list) = 
	List.iter incchildren ops;
	List.iter (fun o -> match o.operation with
	| Result (_,_) -> incoperationcount o
	| _ -> ()) ops;
	List.iter (fun c -> match c.requires with
		| None -> ()
		| Some (o,_) -> incoperationcount o
	) cs

(* generate operation counts for all modules *)
let generateoperationcounts (f:funmodule) = 
	List.iter (fun m -> dooperationcounts m.moutputs m.mdataFlowGraph) f.vmodules

(* remove unreferenced ops, update children, repeat *)
let rec pruneoperations (os:voperation list) =
	if List.exists (fun o -> o.ousecount = 0) os
	then pruneoperations (List.filter (fun o -> if o.ousecount = 0 then (decchildren o; false) else true) os)
	else os

(* gives whether the children of o are in l, or default if o has no children *)
let childreninlist (default:bool) (o:voperation) (l:voperation list) = 
	match o.operation with
	| Result (_,o1) -> List.memq o1 l
	| Unary (_,o1,_) -> List.memq o1 l
	| Binary (_,o1,o2,_) -> List.memq o1 l && List.memq o2 l
	| _ -> default

(* removes duplicate operations *)
let rec compactoperations (c:vconnection list) (acc:voperation list) (skip:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> (match skip with
		| [] -> acc
		| _ -> compactoperations c acc [] skip
	)
	| h::t -> if(childreninlist true h acc)
		then
			let (eqs,neqs) = 
				List.partition (fun o -> eq_operation_type h.operation o.operation) t
			in let replacements = List.map (fun o -> (o,h)) eqs
			in  replaceoperations replacements acc;
				replaceoperations replacements neqs;
				replaceoperations replacements skip;
				replaceconditions replacements c;
				compactoperations c (h::acc) skip neqs
		else compactoperations c acc (h::skip) t

(* optimises away unecessary operations *)
let culloperations (f:funmodule) = List.iter
	(fun m -> 
		m.mdataFlowGraph <- pruneoperations m.mdataFlowGraph
	) f.vmodules;
	List.iter (fun m ->
		m.mdataFlowGraph <- compactoperations m.moutputs [] [] m.mdataFlowGraph
	) f.vmodules;;

(* gets the children of an op *)
let getchildren (o:voperation) = 
	match o.operation with
	| Result (_,o1) -> [o1]
	| Unary (_,o1,_) -> [o1]
	| Binary (_,o1,o2,_) -> [o1;o2]
	| _ -> []

(* gets the number of steps an operation takes *)
let operationoffset (o:voperation) = match o.operation with
	| Unary (Cast,_,_) -> 0 (* cast is instant *)
	| Unary (_,_,_)  
	| Binary(_,_,_,_) -> 1 (* other operators take 1 step *)
	| _ -> 0 (* results, consts and variables are instant *)

(* sets the asap schedule for o *)
let asap (o:voperation) = o.oschedule <- 
	{
		earliest = (List.fold_left max 0 (List.map (fun o1 -> o1.oschedule.earliest + (operationoffset o1)) (getchildren o)));
		latest = o.oschedule.latest; 
		set = o.oschedule.set;
	}

(* sets the alap schedule for o*)
let alap (latest:int) (ops:voperation list) (o:voperation) = o.oschedule <- 
	{
		earliest = o.oschedule.earliest; 
		latest = (let users = List.filter (fun o1 -> List.memq o (getchildren o1)) ops 
			in match users with
				| [] -> latest
				| h::t -> (List.fold_left min h.oschedule.latest (List.map (fun o1 -> o1.oschedule.latest - (operationoffset o1)) t))
		);
		set = o.oschedule.set;
	}

(* builds asap schedule for ops (start with acc=[]) *)
let rec generateasap (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (ready,notready) = List.partition (fun o -> childreninlist true o acc) ops
		in List.iter asap ready;
			generateasap (List.rev_append ready acc) notready
(* builds alap schedule for ops (start with acc=[] and latest as the maximum asap value) *)
let rec generatealap (latest:int) (acc:voperation list) (ops:voperation list) = 
	match ops with
	| [] -> ()
	| _ -> let (notready,ready) = List.partition (fun o -> List.exists (fun o1 -> List.memq o (getchildren o1)) ops) ops
		in List.iter (alap latest acc) ready;
			generatealap latest (List.rev_append ready acc) notready

(* generates an overall schedule for the module *)
let rec generateschedule (m:vmodule) =
	List.iter (fun o -> o.oschedule <- (* TODO add non trivial scheduler *)
	{earliest=o.oschedule.earliest;latest=o.oschedule.latest;set=o.oschedule.earliest}) m.mdataFlowGraph

(* generates schedules for all modules *)
let generatescheduleinfo (f:funmodule) = 
	List.iter (fun m -> 
		generateasap [] m.mdataFlowGraph; 
		generatealap (List.fold_left (fun a b -> max a b.oschedule.earliest) (* find max time *)
			0 m.mdataFlowGraph) [] m.mdataFlowGraph;
		generateschedule m;
	) f.vmodules

(* removes all stmts with no preds, excluding the first statement *)
let extractminentry (ss:stmt list) :stmt list = 
	match ss with 
		| [] -> E.s (E.error "Empty function body")
		| h::t -> h::(List.filter (fun s -> (List.length s.preds) <> 0) t)

(* turns a cil fundec into a vil funmodule *)
let funtofunmodule (f:fundec) :funmodule = dataid := 0; (* Reset op ids *)
	(* generate variable for function *)
	let vardesc = generatedesc f.svar
	(* basic starting point *)
	in let ret = {
		vdesc = vardesc;
		vinputs = generatevariables f.sformals;
		vlocals = generatevariables f.slocals;
		vmodules = List.map (generatemodule vardesc) (extractminentry f.sallstmts);
	} in begin
		(* Generate connections for compacting to use *)
		generateconnections ret;
		(* Compact modules to a minimal set maintaining the same control flow *)
		ret.vmodules <- compactmodules [] ret.vmodules;
		(* Remove unused variables *)
		variablecull ret;
		(* Set up usage pointers for operation graphs *)
		generateoperationcounts ret;
		(* remove unused operations *)
		culloperations ret;
		(* schedule *)
		generatescheduleinfo ret;
		(* return the result *)
		ret
	end

