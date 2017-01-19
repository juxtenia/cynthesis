open Cil
open Feature
module V = Vil
module E = Errormsg

let printlevel = ref 16;;

let cynthesize f = 
	Simplify.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	Partial.makeCFGFeature.fd_enabled <- true; (* Stops the partial feature failing *)
	Partial.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	if(!printlevel land 2 <> 0) then Printers.cfgfeature.fd_doit f else ();      (** DEBUG PRINT *)
	if(!printlevel land 4 <> 0) then Printers.cfglistfeature.fd_doit f else ();  (** DEBUG PRINT *)
	if(!printlevel land 1 <> 0) then Printers.transfeature.fd_doit f else ();    (** DEBUG PRINT *)
	if Validitycheck.check f 
	then List.iter (fun glob -> match glob with
    	| GFun(fd,_) when fd.svar.vinline ->  let result = V.funtofunmodule fd 
  			in  if(!printlevel land 8 <> 0) then E.log("%s\n") (V.string_of_funmodule result) else ();
  				if(!printlevel land 16 <> 0) then E.log("%s\n") (V.print_funmodule result) else ();
      			fd.svar.vinline <- false (* Temporary hack to stop gcc having a hissy fit*)
    	| _ -> ()
  	) f.globals
    else E.log("There were errors \n")

let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [("--cynthesis_print_flags",
    	Arg.Set_int printlevel,
    	" print flags for the cynthesis plugin\n" ^
		"  1 - Print code before cynthesis\n" ^ 
		"  2 - Print CFG info\n" ^
		"  4 - Print a detailed CFG list\n" ^ 
		"  8 - Dump all info about resulting module\n" ^
		"  16 - Print resulting module\n")
    ];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = cynthesize;
    fd_post_check = false;
}

let () = Feature.register feature