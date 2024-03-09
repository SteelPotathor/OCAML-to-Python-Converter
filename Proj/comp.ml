
(* Main function and target of compilation in Makefile *)

let run infile =
  let p = (Interf.parse infile) in
  let _ = (Typing.tp_prog p) in
  let trp = Transf.transf_prog p in
  Pprinter.print_prog trp

let main () = 
  print_string "bonjour\n"
;;

(* TODO: replace by the following once fct. run is implemented:
let main () =
  run Sys.argv.(1) 
;;
*)
    
main();;

