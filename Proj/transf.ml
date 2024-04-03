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
        | IfThenElse(e1,e2,e3) -> not(StringSet.mem f (names_expr e1)) && is_tailrec_expr f e2 && is_tailrec_expr f e3
        | e -> not(StringSet.mem f (names_expr e));;

                            

(* à tester, potentiel pb sur callE, un appel n'est pas forcément récursif? *)
let rec convert_tailrec l e = 
    let rec aux = function
        | IfThenElse(e1, e2, e3) -> Cond(e1, aux e2, aux e3)
        | CallE(a::b) -> Assign(l, b)
        | x -> Return(x)
    in aux e;;

exception NotTailRec;;

(* transform an expression e into a command if f is tail recursive *)
let transf_expr f l e = 
    if is_tailrec_expr f e then
        While(Const(BoolV(true)), convert_tailrec l e) 
    else raise NotTailRec;;

(* transform a function/procedure definition *)
let transf_fpdefn = function
| Fundefn(FPdecl(tp, name, l), e) -> try Procdefn(FPdecl(tp, name, l), transf_expr name (List.map name_of_vardecl l) e) with NotTailRec -> Fundefn(FPdecl(tp, name, l), e)
| Procdefn _ as p -> p;;  

(* transform a program *)
let transf_prog Prog(fdfs, e) = Prog(List.map transf_fpdefn fdfs, e);;
