open Cil
module E = Errormsg

let pass = ref true;;

class validatorVisitor = object(self)
	inherit nopCilVisitor 

	method vvdec (v:varinfo) =
	    (match v.vstorage with
	    | Static -> pass := false; E.log "%a: ERROR Can't use static variables in hardware function.\n" d_loc v.vdecl
	    | Extern -> pass := false; E.log "%a: ERROR Can't use extern variables in hardware function.\n" d_loc v.vdecl
	    | _ -> ()
	    );
		if v.vglob && (match v.vtype with 
			| TFun(_,_,_,_) -> false 
			| _ -> true) 
		then (pass := false; E.log "%a: ERROR Can't use global variable %s in hardware function.\n" d_loc v.vdecl v.vname; DoChildren)
		else (DoChildren)

	method vvrbl (v:varinfo) = 
		(match v.vstorage with
	    | Static -> pass := false; E.log "%a: ERROR Can't use static variables in hardware function.\n" d_loc v.vdecl
	    | Extern -> pass := false; E.log "%a: ERROR Can't use extern variables in hardware function.\n" d_loc v.vdecl
	    | _ -> ()

	    );
		if v.vglob && (match v.vtype with 
			| TFun(_,_,_,_) -> false 
			| _ -> true) 
		then (pass := false; E.log "%a: ERROR Can't use global variable %s in hardware function.\n" d_loc v.vdecl v.vname; SkipChildren)
		else SkipChildren

	method vexpr (e:exp) = 
		match e with
		| SizeOf (_) -> pass := false; E.log "ERROR sizeof should have been optimised away! At %a\n" d_exp e; DoChildren
		| SizeOfE (_) -> pass := false; E.log "ERROR sizeof should have been optimised away! At %a\n" d_exp e; DoChildren
		| SizeOfStr (_) -> pass := false; E.log "ERROR sizeof should have been optimised away! At %a\n" d_exp e; DoChildren
		| AlignOf (_) -> pass := false; E.log "ERROR Can't use AlignOf in hardware function. At %a\n" d_exp e; DoChildren
		| AlignOfE (_) -> pass := false; E.log "ERROR Can't use AlignOf in hardware function. At %a\n" d_exp e; DoChildren
		| AddrOf (_) -> pass := false; E.log "ERROR Can't use Pointer types in hardware function. At %a\n" d_exp e; DoChildren
		| AddrOfLabel (_) -> pass := false; E.log "ERROR Can't use Pointer types in hardware function. At %a\n" d_exp e; DoChildren
		| _ -> DoChildren

	method vinst (i:instr) =
		match i with
		| Call(_,_,_,l) -> pass := false; E.log "%a: ERROR Can't call other functions in hardware function.\n" d_loc l; DoChildren
		| Asm(_,_,_,_,_,l) -> pass := false; E.log "%a: ERROR Can't use inline assembly in hardware function.\n" d_loc l; DoChildren
		| _ -> DoChildren

	method vstmt (s:stmt) =
		List.iter (fun l -> match l with
			| CaseRange (_,_,l) -> pass := false; E.log "%a: ERROR Can't use CaseRange in hardware function.\n" d_loc l
			| Case (_,l) -> pass := false; E.log "%a: ERROR Case label should have been optimised away.\n" d_loc l
			| Default l -> pass := false; E.log "%a: ERROR Default label should have been optimised away.\n" d_loc l
			| _ -> ()
		) s.labels;
		(match s.skind with
			| ComputedGoto (_,l) -> pass := false; E.log "%a: ERROR Can't use ComputedGoto in hardware function.\n" d_loc l
			| TryFinally (_,_,l) -> pass := false; E.log "%a: ERROR Can't use TryFinally in hardware function.\n" d_loc l
			| TryExcept (_,_,_,l) -> pass := false; E.log "%a: ERROR Can't use TryExcept in hardware function.\n" d_loc l
			| Switch (_,_,_,l) -> pass := false; E.log "%a: ERROR Switch should have been optimised away.\n" d_loc l
			| _ -> ()
		);
		DoChildren

	method vtype (t:typ) =
		match t with
		| TInt(IBool,_) -> pass := false; E.log "ERROR Can't use bool type %a in hardware function.\n" d_type t; DoChildren
		| TFloat(_,_) -> pass := false; E.log "ERROR Can't use floating point type %a in hardware function.\n" d_type t; DoChildren
		| TPtr(_,_) -> pass := false; E.log "ERROR Can't use pointer type %a in hardware function.\n" d_type t; DoChildren
		| TArray(_,_,_) -> pass := false; E.log "ERROR Can't use array type %a in hardware function.\n" d_type t; DoChildren
		| TFun(_,_,true,_) -> pass := false; E.log "ERROR Hardware function can't be varargs type %a.\n" d_type t; DoChildren
		| _ -> DoChildren
	
	method vfunc (f:fundec) =
		if f.svar.vinline then DoChildren
		else SkipChildren
end

let check (f:file) :bool =
	let vald = new validatorVisitor
	in List.iter (fun g -> match g with
		| GFun(f,_) -> visitCilFunction vald f; ()
		| _ -> ();
	) f.globals; !pass;;