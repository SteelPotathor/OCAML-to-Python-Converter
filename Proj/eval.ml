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

let eval_barith b v1 v2 boo = match v1, v2 with | Val(IntV i1), Val(IntV, i2) -> if boo && v2 = 0 then failwith "Division par zéro" else Val(IntV(op v1 v2)) 
                                                | _ -> failwith "L'opération " ^ b ^ " s'effectue entre deux entiers"));;

let rec eval_expr env = function 
        | Const v -> Val v
        | VarE v -> (match List.assoc_opt v env with | Some s -> s | _ -> failwith "Variable " ^ v ^ " non présente dans l'environnement")
        | BinOp (b, e1, e2) -> let v1 = eval_expr env e1 and e2 = eval_expr env e2 in (match b with | BArith BAadd -> eval_barith (+) v1 v2 false
                                                                                                    | BArith BAsub -> eval_barith (-) v1 v2 false
                                                                                                    | BArith BAmul -> eval_barith ( * ) v1 v2 false
                                                                                                    | BArith BAdiv -> eval_barith (/) v1 v2 true
                                                                                                    | BArith BAmod -> eval_barith (%) v1 v2) false
        | IfThenElse(e1, e2, e3) -> (match eval_expr e1 with | Val(BoolV true) -> eval_expr env e2
                                                             | Val(BoolV false) -> eval_expr env e3
                                                             | _ -> failwith "La condition n'est pas un booléen")
        | CallE(a::b) -> let eval_param = List.map (eval_expr env) b and Some(Closure(fp, _)) = eval_expr a in match fp with | Fundefn() (* je sais pas *) (* pas assez d'args, on crée une closure avec le nombre restant, trop = erreur, parfait = evalutation *)
    ;;

(* Work in progress, almost done*)
let eval_fpdefn env = function
        | Fundefn(FPdecl(tp, name, l), e) -> (name, eval_expr e) :: env
        | Procdefn -> env (* to implement ? *);;

(* Work in progress => use list.folr to acc into the env and evaluate the expr with this env, seems okay i guess *)
let eval_prog (Prog (fdfs, e)) = let env = List.fold_right (fun x f -> eval_fpdefn x f) [] fdfs in Val(eval_expr env e);;
;;
