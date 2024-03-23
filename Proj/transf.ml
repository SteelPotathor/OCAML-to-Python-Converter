  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String);

(* StringSet of all names contained in expr *)
let rec names_expr = function
        | Const _ -> StringSet.empty
        | VarE e -> StringSet.singleton e
        | CallE l -> StringSet.of_list List.map (names_expr l)
        | BinOp(op, e1, e2) -> StringSet.union (names_expr e1) (names_expr e2)
        | IfThenElse(cond, e1, e2) -> StringSet.union cond (StringSet.union (names_expr e1) (names_expr e2));;

(* check if an expr is tail recursive *)
let rec is_tailrec_expr f = function
        | VarE e -> f <> e
        | CallE(a::b) -> not(StringSet.mem f (names_expr b))
        | BinOp(op,e1,e2) -> not(StringSet.mem f names_expr(e1)) && not(StringSet.mem f names_expr(e2))
        | IfThenElse(e1,e2,e3) -> not(StringSet.mem f (names_expr e1) && is_tailrec_expr f e2 && is_tailrec_expr f e3
        | e -> not(StringSet.mem f (names_expr e));;

                            

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

(* brouillon incorrect *)
let rec convert_tailrec l e = 
    let rec aux = function
        | IfThenElse(e1, e2, e3) -> Cond(e1, aux e2, aux e3)
        | CallE(a::b) -> Assign(l, b)
        | x -> Return(x)
    in aux e;;

(* transf_expr que faire si f n'est pas tail rec? doit on quand même transformer en cmd? *)
let rec convert = function
| BinOP(op, e1, e2) -> Return()
| IfThenElse(e1, e2, e3) -> Cond(e1, convert e2, convert e3)
| CallE

(* transform a tail recursive expression into a command (not recursive) *)
let transf_expr f l e = 
    if is_tailrec_expr f e then
        While(Const(BoolV(true)), convert_tailrec l e) 
    else f (* que faire dans le else? *);;

let transf_fpdefn = function
| Fundefn((tp, name, l), e) -> transf_expr name (List.map (name_of_vardecl) l) e
| Procdefn((tp, name, l), c) -> c;;  

let transf_prog Prog(l, e) = Prog(List.map (transf_fpdefn) l), transf_expr e)   



(* recuperer toutes les vraibales d'une epxr et vérifier que f n'est pas présent 

header => juste virer le type de retour

transformation => rien bouger si recursif non terminale

tout tester, ne pas prendre de risques *)