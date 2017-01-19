open Vil
open Vast

let vil_to_vast (f:funmodule):vastmodule = let ret = {
		modname = f.vdesc.varname;
		inputs = [];
		outputs = [];
		locals = [];
		always = [];
		clockedge = [];
	}
	in ret;;