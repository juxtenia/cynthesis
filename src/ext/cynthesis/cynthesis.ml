open Cil
open Feature
module V = Vil
module E = Errormsg

let cynthesize f = 
	Simplify.feature.fd_doit f;
	Partial.makeCFGFeature.fd_doit f;
	Partial.makeCFGFeature.fd_enabled <- true; (* Stops the partial feature failing *)
	Partial.feature.fd_doit f;
	Printers.cfgfeature.fd_doit f;   (** DEBUG PRINT *)
	Printers.transfeature.fd_doit f; (** DEBUG PRINT *)
	if Validitycheck.check f 
	then List.iter (fun glob -> match glob with
      | GFun(fd,_) when fd.svar.vinline ->  E.log("%s\n") (V.string_of_funmodule (V.funtofunmodule fd));
      	fd.svar.vinline <- false; (* Temporary hack to stop gcc having a hissy fit*)
      | _ -> ()
  	) f.globals
	else E.log("There were errors \n")

let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = cynthesize;
    fd_post_check = false;
}

let () = Feature.register feature