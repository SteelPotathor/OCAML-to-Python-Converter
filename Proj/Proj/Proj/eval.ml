open Lang

(* ************************************************************ *)
(* ****  Operational Semantics                             **** *)
(* ************************************************************ *)

(* ****  Auxiliary functions                               **** *)


(* ****  Result of evaluation                              **** *)

type result =
    Val of value
  | Closure of fpdefn * (result list)

type state = (vname * result) list

      
(* TODO: implement *)
let eval_prog (Prog (fdfs, e)) = Val (IntV 42)
;;
