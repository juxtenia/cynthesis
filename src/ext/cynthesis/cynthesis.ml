open Cil
open Feature
module E = Errormsg

let feature = 
  { fd_name = "cynthesis";
    fd_enabled = false;
    fd_extraopt = [];
    fd_description = "verilog HLS of functions marked with 'inline'";
    fd_doit = (fun f -> E.log (if Validitycheck.check f then "Pass\n" else "Fail\n" ));
    fd_post_check = false;
}

let () = Feature.register feature