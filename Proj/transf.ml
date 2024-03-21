  (* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

let present f e = 
    let rec aux = function 
                        | VarE(e) -> f <> e 
                        | CallE(a::b) -> aux a && aux b 
                        | _ -> true
in aux e;;

(* f = v name; e = expr *)
let rec is_tailrec_expr f = function 
                            | IfThenElse(e1, e2, e3) -> match e1 with CallE(a) -> present f a | _ -> true && is_tailrec_expr f e2 && is_tailrec_expr f e3 
                            | CallE(a::b) -> present f b 
                            | _ -> true (* on ne peut jamais croiser un VarE sinon mauvais typage ni d'opérations (même raison) *);;
                            
(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)

(* brouillon correct *)
let rec convert f l = function
| IfThenElse(e1, e2, e3) -> Cond(e1, convert f l e2, convert f l e3)
| CallE(a::b) -> if f = a then Assign(l, b) else Skip  (* clarification par rapport à l'enoncé requise, skip? seq? *)
| x -> Return(x);;

let transf_expr f l e = if is_tailrec_expr f e then While(Const(BoolV(true)), convert f l e) else failwith "la fonction n'est pas récursive terminale";;

let transf_fpdefn = function
| Fundefn((tp, name, l), e) ->(* def? quel type use*) transf_expr name l e
| Procdefn((tp, name, l), e) -> e;;  

let transf_prog = 

let rec factr (n : int) (acc: int) : int =
if n = 0
then acc
else factr (n - 1) (n * acc)
;;
factr 5 1;;

def factr (n,acc) :
while True :
if (n == 0) :
return(acc)
else :
(n,acc )=((n - 1),(n * acc ))
factr (5 ,1)
