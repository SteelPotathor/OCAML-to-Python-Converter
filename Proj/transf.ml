  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

let rec present f e = 
    let rec aux = function 
                        | VarE(e) -> f <> e 
                        | CallE(a::b) -> aux a && aux b 
                        | _ -> true
in aux e;;

(* f = v name; e = expr *)
let is_tailrec_expr f = function 
                            | IfThenElse(e1, e2, e3) -> match e1 with CallE(a) -> present f a && is_tailrec_expr f e2 && is_tailrec_expr f e3 
                            | CallE(a::b) -> present f b 
                            | _ -> true (* on ne peut jamais croiser un VarE sinon mauvais typage ni d'opérations (même raison) *);;
                            
(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

