open Big_int
open Cil
module E = Errormsg

(** Unary Operations*)
type unop =
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
	mutable connectfrom: int;
		(** The id of the exporting module *)
	mutable connectto: int;
		(** The id of the importing module *)
	mutable requires: (int * bool) option;
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
	| VARIABLE of vvarinfo
		(** The value of a variable as it enters the module *)
	| CONSTANT of vconstinfo
		(** The value of a constant *)
	| RESULT of vvarinfo * voperation
		(** marks the value of a variable that should be passed to the next 
			stage *)
	| UNARY of unop * voperation * vtype
		(** applies a unary operation to the previous item *)
	| BINARY of binop * voperation * voperation * vtype
		(** applies a binary operation to the previous items *)  
and vtype = {
	mutable width: int;
		(** The width of the type, in bits *)
	mutable isSigned: bool;
		(** Whether this is a signed variable *)
}

let blanktype :vtype = {width = 0; isSigned = false};;

let rec typesigned (t:typ) :bool = match t with
	| TInt(ik,_) -> isSigned ik
	| TNamed(ti,_) -> typesigned ti.ttype
	| TComp(_,_) -> false
	| TEnum(_,_) -> false
	| _ -> E.s (E.error "ERROR Illegal type %a.\n" d_type t)

let rec generatetype (t:typ) :vtype = 
	{
		width = bitsSizeOf t;
		isSigned = typesigned t;
	}

let generatedesc (d:varinfo) :vvarinfo =
	match d.vtype with
	| TFun(t,Some a, false, _) -> 
		{ varname = d.vname; vtype = (generatetype t) }
	| _ -> E.s (E.error "ERROR Illegal type %a.\n" d_type d.vtype) 

let generatevariable (v:varinfo) :vvarinfo = 
	{
		varname = v.vname;
		vtype = generatetype v.vtype;
	}

let generatevariables (vs:varinfo list) :vvarinfo list =
	List.map generatevariable vs

let generatemodule (s:stmt) :vmodule = 
	let ret = 
	{
		mid = s.sid;
		minputs = [];
		moutputs = [];
		mdataFlowGraph = [];
	} in begin 
		(match s.skind with
			| Instr (il) -> ()
  			| Return (eo,l) -> ()
  			| Goto (sr,l) -> ()
  			| Break (l) -> ()
  			| Continue (l) -> ()
  			| If (e,b1,b2,l) -> ()
  			| Loop (b,l,so1,so2) -> ()
  			| Block (b) -> ()
  			| _ -> E.s (E.error "ERROR Illegal stmt %a.\n" d_stmt s) 
  		);
  		ret
	end

let generateconnections (f:fundec) (m:funmodule) = ()


let funtofunmodule (f:fundec) :funmodule = let ret = {
		vdesc = generatedesc f.svar;
		vinputs = generatevariables f.sformals;
		vlocals = generatevariables f.slocals;
		vcontrolconnections = [];
		vmodules = List.map generatemodule f.sallstmts;
	} in begin
		generateconnections f ret; 
		ret
	end

let rec string_of_unop u = match u with
  | Neg -> "Neg"
  | BNot -> "BNot"
  | LNot -> "LNot"
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
	"{ connectfrom:" ^ (string_of_int c.connectfrom)
	^ ", connectto:" ^ (string_of_int c.connectto)
	^ ", requires:" ^ (match c.requires with 
		| Some (i,b) -> "Some:(" ^ (string_of_int i) ^ ", " ^ (string_of_bool b) ^ ")"
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
	| VARIABLE v -> "Variable:" ^ (string_of_vvarinfo v)
	| CONSTANT c -> "Constant:" ^ (string_of_vconstinfo c)
	| RESULT (v,o) -> "Result:(" ^ (string_of_vvarinfo v) ^ ", " ^ (string_of_int o.oid) ^ ")"
	| UNARY (u,o,t) -> "Unary:(" ^ (string_of_unop u) ^ ", " ^ (string_of_int o.oid) ^ ", " 
		^ (string_of_vtype t) ^ ")" 
	| BINARY (u,o1,o2,t) -> "Binary:(" ^ (string_of_binop u) ^ ", " ^ (string_of_int o1.oid) 
		^ ", " ^ (string_of_int o2.oid) ^ ", " ^ (string_of_vtype t) ^ ")" 
	) ^ "}"
and string_of_vtype t = 
	"{ width:" ^ (string_of_int t.width)
	^ ", isSigned:" ^ (string_of_bool t.isSigned)
	^ "}"