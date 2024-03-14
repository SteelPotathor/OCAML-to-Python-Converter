
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}

exception NotFound;;

let tp_const = function
    | BoolV _ -> BoolT
    | IntV _ -> IntT;;

let rec search_tp_localvar v env = let rec aux = function
	|(a, b)::c -> if v = a then b else aux c
    |_ -> raise NotFound
in aux env;; 

(* AIDE Tim => (vname * fpdecl = tp * vname * (vardecl list = vname * tp) list) c'est quoi fpdecl? *)
let search_tp_funbind v env = let rec aux = function
	|(a, b)::c -> if v = a then fst b else aux c
	|_ -> raise NotFound
in aux env;; 

let tp_var env v = try search_tp_var v env.localvar with | NotFound -> try search_tp_funbind v env.funbind with | NotFound -> failwith "echec de typage, la variable n'est pas dans l'environnement";;

let rec function_type_correct = fun (tf, targs) -> match tf,targs with FunT (a,b),(ta::reste) -> a=ta && function_type_correct(b, reste)
                                                     |_ -> true;;
let rec tp_expr env = function
    | Const c -> tp_const c 
    | VarE v -> tp_var env v
    | BinOp(b, e1, e2) -> match b with | BArith b -> let t1 = tp_expr env e1 in if t1 = IntT && t1 = tp_expr env e2 then t1 else failwith "echec de typage, les opérations arithmétiques se font sur des int" 
                                       | BCompar b ->  if tp_expr env e1 = tp_expr env e2 then BoolT else failwith "echec de typage, les comparaisons doivent être de même type"
                                       | BLogic b -> let t1 = tp_expr env e1 in if t1 = BoolT && t1 = tp_expr env e2 then BoolT else failwith "echec de typage, les opérations logiques se font sur des bool"
    | IfThenElse(e1, e2, e3) -> (if tp_expr env e1 = BoolT then let t2 = tp_expr env e2 in (if t2 = tp_expr env e3 then t2 else failwith "echec de typage, then et else n'ont pas le même type") else failwith "echec de typage, if n'est pas un booléen")
    | CallE el -> let tl=List.map tp_expr el in  

(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT
