open Vil
open Vast

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

let vil_to_vast_module (r:vastmodule) (m:vmodule) =
	()

let vil_to_vast (f:funmodule):vastmodule = let ret = {
		modname = f.vdesc.varname;
		inputs =
			(vil_to_vast_variable NA {varname="start"; vtype={width=1; isSigned=false}}) 
			:: (List.map (vil_to_vast_variable NA) f.vinputs);
		outputs = 
			(vil_to_vast_variable REG {varname="finish"; vtype={width=1; isSigned=false}}) 
			:: (vil_to_vast_variable REG f.vdesc) :: [];
		locals = List.map (vil_to_vast_variable NA) f.vlocals;
		always = [];
		clockedge = [];
	}
	in  List.iter (vil_to_vast_module ret) f.vmodules;
		ret;;