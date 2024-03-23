type vname = string

(* binary arithmetic operators: +, -, *, /, mod *)
type barith = BAadd | BAsub | BAmul | BAdiv | BAmod

(* binary comparison operators: =, >=, >, <=, <, != *)
type bcompar = BCeq | BCge | BCgt | BCle | BClt | BCne

(* binary logic operators *)
type blogic = BLand | BLor

(* binary operators, combining all of the above *)
type binop =
    BArith of barith
  | BCompar of bcompar
  | BLogic of blogic

type value =
    BoolV of bool
  | IntV of int
  | UndefV
    
(* expresssions *)
type expr = 
    Const of value                     (* constant *)
  | VarE  of vname                     (* variable *)
  | BinOp of binop * expr * expr       (* binary operation *)
  | IfThenElse of expr * expr * expr   (* if - then - else *)
  (* function call. The parser ensures that the expr list is always non-empty *)
  | CallE of (expr list)

(* commands *)
type cmd =
    Skip                                  (* empty command, does nothing *)
  | Assign of (vname list) * (expr list)  (* parallel assignment *)
  | Seq of cmd * cmd                      (* sequence of commands *)
  | Cond of expr * cmd * cmd              (* conditional command(!), compare with IfThenElse *)
  | While of expr * cmd                   (* while loop *)
  | Return of expr                        (* return from current procedure *)

(* Types *)
type tp = BoolT | IntT | FunT of tp * tp

let numeric_tp = function
    IntT -> true
  | _ -> false

let base_tp = function
    FunT(_, _) -> false
  | _ -> true

(* variable / parameter declaration *)
type vardecl = Vardecl of vname * tp

let name_of_vardecl (Vardecl (vn, _)) = vn

let tp_of_vardecl (Vardecl (_, t)) = t

(* function declaration: return type; parameter declarations *)
type fpdecl = FPdecl of tp * vname * (vardecl list)

let name_of_fpdecl (FPdecl (t, fn, pds)) = fn
let params_of_fpdecl (FPdecl (t, fn, pds)) = pds

(* function definition: function declaration; local var decls; function body *)
type fpdefn =
    Fundefn of fpdecl * expr  (* function definition:  header, expression *)
  | Procdefn of fpdecl * cmd  (* procedure definition: header, command *)

let fpdecl_of_fpdefn = function
  Fundefn(fd, _) -> fd
  | Procdefn (fd, _) -> fd
let name_of_fpdefn fpd = name_of_fpdecl (fpdecl_of_fpdefn fpd)

(* program: function definitions and expression to be evaluated *)
type prog = Prog of (fpdefn list) * expr
