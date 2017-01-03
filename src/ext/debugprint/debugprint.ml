open Cil
open Feature
module E = Errormsg


let string_of_char = String.make 1 ;;

let ikind_structure (i:ikind) :string =
	match i with
	| IChar -> "IChar"
	| ISChar -> "ISChar"
	| IUChar -> "IUChar"
	| IBool -> "IBool"
	| IInt -> "IInt"
	| IUInt -> "IUInt"
	| IShort -> "IShort"
	| IUShort -> "IUShort"
	| ILong -> "ILong"
	| IULong -> "IULong"
	| ILongLong -> "ILongLong"
	| IULongLong -> "IULongLong"
;;

let fkind_structure (f:fkind) :string =
	match f with
	| FFloat -> "FFloat"
	| FDouble -> "FDouble"
	| FLongDouble -> "FLongDouble"
;;

let storage_structure (s:storage) : string = 
	match s with
	| NoStorage -> "NoStorage"
	| Static -> "Static"
	| Register -> "Register"
	| Extern  -> "Extern "
;;

let unop_structure (u:unop) : string =
	match u with
	| Neg -> "Neg"
	| LNot -> "LNot"
	| BNot -> "BNot"
;;

let binop_structure (b:binop) : string =
	match b with
	| PlusA -> "PlusA"
	| PlusPI -> "PlusPI"
	| IndexPI -> "IndexPI"
	| MinusA -> "MinusA"
	| MinusPI -> "MinusPI"
	| MinusPP -> "MinusPP"
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
	| LAnd -> "LAnd"
	| LOr -> "LOr"

let location_structure (l:location) :string =
	"location:{line:" ^ (string_of_int l.line) 
	^ ", file:\"" ^ l.file
	^ "\", byte:" ^ (string_of_int l.byte)
	^ "}"
;;

let comment_structure ((l,s):comment) :string =
	"comment:(" ^ (location_structure l) 
	^ ", \"" ^  s
	^ "\")"
;;

let rec attribute_structure ((Attr(s,ap)):attribute) :string =
	"Attr:(\"" ^ s ^ "\", " ^ (attrparam_list_structure ap "") ^ ")"
and attributes_structure (a:attributes) (s:string) :string =
	match a with
	| [] -> s
	| [at] -> s ^ (attribute_structure at)
	| at::tl -> attributes_structure tl (s ^ (attribute_structure at) ^ ", ")
and params_structure (p: (string * typ * attributes) list) (s:string):string =
	match p with
	| [] -> s
	| [(st,t,a)] -> s ^ "(\"" ^ st ^ "\", " ^ (typ_structure t) ^ ", [" ^ (attributes_structure a "") ^ "])"
	| (st,t,a) :: tl -> params_structure tl (s ^ "(\"" ^ st ^ "\", " ^ (typ_structure t) ^ ", [" ^ (attributes_structure a "") ^ "]), ")
and params_option_structure (p: (string * typ * attributes) list option) :string =
	match p with
	| Some pr -> (params_structure pr "[") ^ "]"
	| None -> "None"
and attrparam_structure (a:attrparam) :string = 
	match a with
	| AInt (i) -> "AInt:(" ^ (string_of_int i)^ ")"
	| AStr (s) -> "AStr:(\"" ^ s ^ "\")"
	| ACons (s,a) -> "ACons:(\"" ^ s ^ "\", " ^ (attrparam_list_structure a "") ^ ")"
	| ASizeOf (t) -> "ASizeOf:(" ^ (typ_structure t) ^ ")"
	| ASizeOfE (a) -> "ASizeOfE:(" ^ (attrparam_structure a) ^ ")"
	| ASizeOfS (ts) -> "ASizeOfS:(" ^ (typesig_structure ts) ^ ")"
	| AAlignOf (t) -> "AAlignOf:(" ^ (typ_structure t) ^ ")"
	| AAlignOfE (a) -> "AAlignOfE:(" ^ (attrparam_structure a) ^ ")"
	| AAlignOfS (ts) -> "AAlignOfS:(" ^ (typesig_structure ts) ^ ")"
	| AUnOp (u, a) -> "AUnOp:(" ^ (unop_structure u) 
		^ ", " ^ (attrparam_structure a) ^ ")"
	| ABinOp (b, a1, a2) -> "ABinOp:(" ^ (binop_structure b) 
		^ ", " ^ (attrparam_structure a1) 
		^ ", " ^ (attrparam_structure a2)^ ")"
	| ADot (a, s) -> "ADot:(" ^ (attrparam_structure a) 
		^ ", \"" ^ s ^ "\")"
	| AStar (a) -> "AStar:(" ^ (attrparam_structure a) ^ ")"
	| AAddrOf (a) -> "AAddrOf:(" ^ (attrparam_structure a) ^ ")"
	| AIndex (a1, a2) -> "AIndex:(" ^ (attrparam_structure a1) 
		^ ", " ^ (attrparam_structure a2) ^ ")"
	| AQuestion (a1 , a2 , a3) -> "AQuestion:(" ^ (attrparam_structure a1) 
		^ ", " ^ (attrparam_structure a2) 
		^ ", " ^ (attrparam_structure a3) ^ ")"
and attrparam_list_structure (a:attrparam list) (s:string) :string =
	match a with
	| [] -> s
	| [at] -> s ^ (attrparam_structure at)
	| at :: tl -> attrparam_list_structure tl (s ^ (attrparam_structure at) ^ ", ")
and typ_structure (t:typ) :string =
	match t with
	| TVoid a -> "TVoid:([" ^ (attributes_structure a "") ^ "])"
	| TInt (k,a) -> "TInt:(" ^ (ikind_structure k)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TFloat (k,a) -> "TFloat:(" ^ (fkind_structure k)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TPtr (t,a) -> "TPtr:(" ^ (typ_structure t)
		^ ", [" ^ (attributes_structure a  "") ^ "])"
	| TArray (t,e,a) -> "TArray:(" ^ (typ_structure t)
		^ ", " ^ (exp_option_structure e)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TFun (t,p,b,a) -> "TFun:(" ^ (typ_structure t)
		^ ", " ^ (params_option_structure p)
		^ ", " ^ (string_of_bool b)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TNamed (t,a) -> "TNamed:(" ^ (typeinfo_structure t)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TComp (c,a) -> "TComp:(" ^ (compinfo_structure c)
		^ ", [" ^ (attributes_structure a "") ^ "])"
	| TEnum (e,a) -> "TEnum:(" ^ (enuminfo_structure e)
		^ ", " ^ (attributes_structure a "") ^ ")"
	| TBuiltin_va_list a -> "TEnum:([" ^ (attributes_structure a "") ^ "])"
and compinfo_structure (c:compinfo) :string =
	"compinfo:{cstruct:" ^ (string_of_bool c.cstruct) 
	^ ", cname:\"" ^ c.cname
	^ "\", ckey:" ^ (string_of_int c.ckey)
	^ ", cfields:[" ^ (fieldinfo_list_structure c.cfields "") ^"]"
	^ ", cattr:[" ^ (attributes_structure c.cattr "") ^ "]" 
	^ ", cdefined:" ^ (string_of_bool c.cdefined)
	^ ", creferenced:" ^ (string_of_bool c.creferenced) ^ "}"
and fieldinfo_structure (f:fieldinfo) =
	"fieldinfo:{fname:\"" ^ f.fname ^"\", " 
	^ ", ftype:" ^ (typ_structure f.ftype) 
	^ ", fbitfield:" ^ (match f.fbitfield with
	| Some i -> string_of_int i
	| None -> "None") 
	^ ", fattr:[" ^ (attributes_structure f.fattr "") ^ "]" 
	^ ", floc:" ^ (location_structure f.floc) ^ "}"
and fieldinfo_list_structure (f:fieldinfo list) (s:string) :string =
	match f with
	| [] -> s
	| [fi] -> s ^ (fieldinfo_structure fi)
	| fi :: tl -> fieldinfo_list_structure tl (s ^ (fieldinfo_structure fi) ^ ", ")
and enuminfo_structure (e:enuminfo) :string =
	"enuminfo:{ename:\"" ^ e.ename 
	^ "\", eitems:[" ^ (eitems_structure e.eitems "")
	^ "], eattr:[" ^ (attributes_structure e.eattr "") 
	^ "], ereferenced:" ^ (string_of_bool e.ereferenced) 
	^ ", ekind:" ^ (ikind_structure e.ekind) ^ "}"
and eitems_structure (e:(string * exp * location) list) (s:string) :string = 
	match e with
	| [] -> s
	| [(s,ex,l)] -> s ^ "(\"" ^ s 
		^ "\", " ^ (exp_structure ex) 
		^ ", " ^ (location_structure l) ^ ")"
	| (s,ex,l) :: tl -> eitems_structure tl (s ^ "(\"" ^ s 
		^ "\", " ^ (exp_structure ex) 
		^ ", " ^ (location_structure l) ^ "), ")
and typeinfo_structure (t:typeinfo) :string =
	"typeinfo:{tname:\"" ^ t.tname 
	^ "\", ttype:" ^ (typ_structure t.ttype) 
	^ ", treferenced:" ^ (string_of_bool t.treferenced) ^ "}"
and varinfo_structure (v:varinfo) :string =
	"varinfo:{vname:\"" ^ v.vname
	^ "\", vtype:" ^ (typ_structure v.vtype) 
	^ ", vattr:[" ^ (attributes_structure v.vattr "")
	^ "], vstorage:" ^ (storage_structure v.vstorage) 
	^ ", vglob:" ^ (string_of_bool v.vglob) 
	^ ", vinline:" ^ (string_of_bool v.vinline) 
	^ ", vdecl:" ^ (location_structure v.vdecl) 
	^ ", vinit:" ^ (initinfo_structure v.vinit)
	^ ", vid:" ^ (string_of_int v.vid) 
	^ ", vaddrof:" ^ (string_of_bool v.vaddrof) 
	^ ", vreferenced:" ^ (string_of_bool v.vreferenced) 
	^ ", vdescrpure:" ^ (string_of_bool v.vdescrpure) ^ "}"	
and exp_option_structure (e:exp option) :string =
	match e with
	| Some ex -> exp_structure ex
	| None -> "None"
and exp_structure (e:exp) :string =
	match e with
	| Const c -> "Const:" ^ (constant_structure c)
	| Lval l -> "Lval:" ^ (lval_structure l)
	| SizeOf t -> "SizeOf:" ^ (typ_structure t)
	| SizeOfE e -> "SizeOfE:" ^ (exp_structure e)
	| SizeOfStr s -> "SizeOfStr:\"" ^ s ^ "\""
	| AlignOf t -> "AlignOf:" ^ (typ_structure t)
	| AlignOfE e -> "AlignOfE:" ^ (exp_structure e)
	| UnOp (u,e,t) -> "UnOp:(" ^ (unop_structure u)
		^ ", " ^ (exp_structure e)
		^ ", " ^ (typ_structure t) ^ ")"
	| BinOp (b,e1,e2,t) -> "BinOp:(" ^ (binop_structure b)
		^ ", " ^ (exp_structure e1)
		^ ", " ^ (exp_structure e2)
		^ ", " ^ (typ_structure t) ^ ")"
	| Question (e1,e2,e3,t) -> "Question:(" ^ (exp_structure e1)
		^ ", " ^ (exp_structure e2)
		^ ", " ^ (exp_structure e3)
		^ ", " ^ (typ_structure t) ^ ")"
	| CastE (t,e) -> "Cast:(" ^ (typ_structure t)
		^ ", " ^ (exp_structure e) ^ ")"
	| AddrOf l -> "AddrOf:" ^ (lval_structure l)
	| AddrOfLabel sr -> "AddrOfLabel:" ^ (stmt_structure !sr)
	| StartOf l -> "StartOf:" ^ (lval_structure l)
and constant_structure (c:constant) :string =
	match c with
	| CInt64 (i,k,so) -> "CInt64:(" ^ (Int64.to_string i)
		^ ", " ^ (ikind_structure k)
		^ ", " ^ (match so with
		| Some s -> "\"" ^ s ^ "\""
		| None -> "None") ^ ")"
	| CStr s -> "CStr:\"" ^ s ^ "\""
	| CWStr (il) -> "[" ^ (String.concat ", " (List.map Int64.to_string il)) ^ "]"
	| CChr c -> string_of_char c
	| CReal (f,k,so) -> "CReal:(" ^ (string_of_float f)
		^ ", " ^ (fkind_structure k)
		^ ", " ^ (match so with
		| Some s -> "\"" ^ s ^ "\""
		| None -> "None") ^ ")"
	| CEnum (e,s,ei) -> "CEnum:(" ^ (exp_structure e)
		^ ", \"" ^ s 
		^ "\", " ^ (enuminfo_structure ei) ^ ")"
and lval_structure ((l,o):lval) :string =
	"lval:(" ^ (lhost_structure l) ^ ", " ^ (offset_structure o) ^ ")"
and lhost_structure (l:lhost) :string =
	match l with
	| Var v -> "Var:" ^ (varinfo_structure v)
	| Mem e -> "Mem:" ^ (exp_structure e)
and offset_structure (o:offset) :string =
	match o with
	| NoOffset -> "NoOffset"
	| Field (fi,o) -> "Field:(" ^ (fieldinfo_structure fi) 
		^ ", " ^ (offset_structure o) ^ ")"
	| Index (e,o) -> "Index:(" ^ (exp_structure e) 
		^ ", " ^ (offset_structure o) ^ ")"
and init_structure (i:init) :string =
	match i with
	| SingleInit e -> "SingleInit:" ^ (exp_structure e)
	| CompoundInit (t, l) -> "CompoundInit:(" ^ (typ_structure t) 
		^ ",[" ^ (String.concat ", " (List.map 
			(fun (o,i) -> "(" ^ (offset_structure o) ^ ", " ^ (init_structure i) ^ ")") l
		)) ^ "])"
and initinfo_structure (i:initinfo) :string =
	match i.init with
	| None -> "initinfo:None"
	| Some s -> "initinfo:{" ^ (init_structure s) ^ "}"
and fundec_structure (f:fundec) :string =
	"fundec:{svar:" ^ (varinfo_structure f.svar)
	^ ", sformals:[" ^ (String.concat ", " (List.map varinfo_structure f.sformals))
	^ "], slocals:[" ^ (String.concat ", " (List.map varinfo_structure f.slocals))
	^ "], smaxid:" ^ (string_of_int f.smaxid)
	^ ", sbody:" ^ (block_structure f.sbody)
	^ ", smaxstmtid" ^ (match f.smaxstmtid with
		| Some i -> string_of_int i
		| None -> "None")
	^ ", sallstmts:[" ^ (String.concat ", " (List.map stmt_structure f.sallstmts)) ^ "]}"
and block_structure (b:block) :string =
	"block:{battrs:" ^ (attributes_structure b.battrs "")
	^ ", bstmts:[" ^ (String.concat ", " (List.map stmt_structure b.bstmts)) ^ "]}"
and stmt_structure (s:stmt) :string =
	"stmt:{labels:[" ^ (String.concat ", " (List.map label_structure s.labels))
	^ "], skind:" ^ (stmtkind_structure s.skind)
	^ ", sid:" ^ (string_of_int s.sid) ^ "}"
and label_structure (l:label) :string =
	match l with
	| Label (s,l,b) -> "Label:(\"" ^ s 
		^ "\", " ^ (location_structure l) 
		^ ", " ^ (string_of_bool b) ^ ")"
	| Case (e,l) -> "Case:(" ^ (exp_structure e) 
		^ ", " ^ (location_structure l) ^ ")"
	| CaseRange (e1,e2,l) -> "CaseRange:(" ^ (exp_structure e1) 
		^ ", " ^ (exp_structure e2) 
		^ ", " ^ (location_structure l) ^ ")"
	| Default l -> "Default:(" ^ (location_structure l) ^ ")"
and stmtkind_structure (s:stmtkind) :string =
	match s with
	| Instr (l) -> "Instr:[" ^ (String.concat ", " (List.map instr_structure l)) ^ "]"
	| Return (e,l) -> "Return:(" ^ (exp_option_structure e) 
		^ ", " ^ (location_structure l) ^ ")"
	| Goto (s,l) -> "Goto:(" ^ (stmt_structure !s)
		^ ", " ^ (location_structure l) ^ ")"
	| ComputedGoto (e,l) -> "ComputedGoto:(" ^ (exp_structure e)
		^ ", " ^ (location_structure l) ^ ")"
	| Break l -> "Break:" ^ (location_structure l)
	| Continue l -> "Continue:" ^ (location_structure l)
	| If (e,b1,b2,l) -> "If:(" ^ (exp_structure e)
		^ ", " ^ (block_structure b1)
		^ ", " ^ (block_structure b2)
		^ ", " ^ (location_structure l) ^ ")"
	| Switch (e,b,_,l) -> "Switch:(" ^ (exp_structure e)
		^ ", " ^ (block_structure b)
		^ ", " ^ (location_structure l) ^ ")"
	| Loop (b,l,_,_) -> "Loop:(" ^ (block_structure b)
		^ ", " ^ (location_structure l)
	| Block b -> "Block:" ^ (block_structure b)
	| _ -> "Unrecognised"
and instr_structure (i:instr) :string =
	match i with
	| Set (lv,e,l) -> "Set:(" ^ (lval_structure lv)
		^ ", " ^ (exp_structure e)
		^ ", " ^ (location_structure l) ^ ")"
	| Call (lvo,e,el,l) -> "Call:" ^ (match lvo with
			| Some lv -> lval_structure lv
			| None -> "None") 
		^ ", " ^ (exp_structure e)
		^ ", [" ^ (String.concat ", " (List.map exp_structure el))
		^ ", " ^ (location_structure l) ^ ")"
	| Asm (_,_,_,_,_,l) -> "Asm:" ^ (location_structure l)
and typesig_structure (ts:typsig) :string =
	match ts with
	| TSArray (t,io,al) -> "TSArray:(" ^ (typesig_structure ts) 
		^ ", " ^ (match io with
			| Some i -> Int64.to_string i
			| None -> "None")
		^ ", [" ^ (attributes_structure al "") ^ "])"
	| TSPtr (ts,al) -> "TSPtr:(" ^ (typesig_structure ts) 
		^ ", [" ^ (attributes_structure al "") ^ "])"
	| TSComp (b,s,al) -> "TSComp:(" ^ (string_of_bool b)
		^ ", \"" ^ s
		^ "\", " ^ (attributes_structure al "") ^ ")"
	| TSFun (ts,tslo,b,al) -> "TSFun:(" ^ (typesig_structure ts)
		^ ", [" ^ (match tslo with
			| Some tsl -> (String.concat ", " (List.map typesig_structure tsl))
				^ "], " ^ (string_of_bool b) 
			| None -> "None"
		) ^ ", [" ^ (attributes_structure al "") ^ "])"
	| TSEnum (s,al) -> "TSEnum:(\"" ^ s 
		^ "\", [" ^ (attributes_structure al "") ^ "])"
	| TSBase (t) -> "TSBase:" ^ (typ_structure t)
;;

let global_structure (g:global) :string =
	match g with 
	| GType (t,l) -> "GType:(" ^ (typeinfo_structure t) 
		^ ", " ^ (location_structure l) ^ ")"
	| GCompTag (c,l) -> "GCompTag:(" ^ (compinfo_structure c) 
		^ ", " ^ (location_structure l) ^ ")"
	| GCompTagDecl (c,l) -> "GCompTagDecl:(" ^ (compinfo_structure c) 
		^ ", " ^ (location_structure l) ^ ")"
	| GEnumTag (e,l) -> "GEnumTag:(" ^ (enuminfo_structure e) 
		^ ", " ^ (location_structure l) ^ ")"
	| GEnumTagDecl (e,l) -> "GEnumTag:(" ^ (enuminfo_structure e) 
		^ ", " ^ (location_structure l) ^ ")"
	| GVarDecl (v,l) -> "GVarTag:(" ^ (varinfo_structure v) 
		^ ", " ^ (location_structure l) ^ ")"
	| GVar (v,i,l) -> "GVarTag:(" ^ (varinfo_structure v) 
		^ ", " ^ (initinfo_structure i)
		^ ", " ^ (location_structure l) ^ ")"
	| GFun (f,l) -> "GFun:(" ^ (fundec_structure f) 
		^ ", " ^ (location_structure l) ^ ")"
	| GAsm (s,l) -> "GAsm:(\"" ^ s ^ "\", " ^ (location_structure l) ^ ")"
	| GPragma (a,l) -> "GPragma:([" ^ (attribute_structure a) 
		^ "], " ^ (location_structure l) ^ ")"
	| GText s -> "GText:\"" ^ s ^ "\""
;;

let rec globals_structure (g:global list) (s:string) :string = 
	match g with
	| [] -> s
	| [gl] -> s ^ (global_structure gl)
	| gl :: gr -> globals_structure gr (s ^ (global_structure gl)^ ", ")
;;

let fundec_option_structure (f:fundec option) :string = 
	match f with 
	| None -> "None"
	| Some fn -> fundec_structure fn
;;

let file_structure (f:file) :string = 
	"file:{fileName:\"" ^ f.fileName
	^ "\", globals:[" ^ (globals_structure f.globals "")
	^ "],globinit:" ^ (fundec_option_structure f.globinit)
	^ ", globinitcalled:" ^ (string_of_bool f.globinitcalled)
	^ "}"
;;

let print (f : file) : unit =
  E.log "%s\n" (file_structure f);
  ()

let feature = 
  { fd_name = "debugprint";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "Dumps a description of the AST to stdout";
    fd_doit = print;
    fd_post_check = false;
}

let () = Feature.register feature
