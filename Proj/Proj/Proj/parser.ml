type token =
  | IDENTIFIER of (string)
  | TP of (Lang.tp)
  | BCONSTANT of (bool)
  | INTCONSTANT of (int)
  | FST
  | SND
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EQ
  | GE
  | GT
  | LE
  | LT
  | NE
  | BLAND
  | BLOR
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | AND
  | ARROW
  | FUN
  | IN
  | LET
  | REC
  | TYPE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lang

let primop_of_token = function
  | ADD ->  (BArith BAadd)
  | SUB ->  (BArith BAsub)
  | MUL ->  (BArith BAmul)
  | DIV ->  (BArith BAdiv)
  | MOD ->  (BArith BAmod)
  | EQ ->  (BCompar BCeq)
  | GE ->  (BCompar BCge)
  | GT ->  (BCompar BCgt)
  | LE ->  (BCompar BCle)
  | LT ->  (BCompar BClt)
  | NE ->  (BCompar BCne)
  | BLAND -> (BLogic BLand)
  | BLOR  -> (BLogic BLor)
  | _ -> failwith "in primop_of_token: unknown token"

let binary_exp e1 oper e2 = BinOp((primop_of_token oper), e1, e2)

# 68 "parser.ml"
let yytransl_const = [|
  261 (* FST *);
  262 (* SND *);
  263 (* ADD *);
  264 (* SUB *);
  265 (* MUL *);
  266 (* DIV *);
  267 (* MOD *);
  268 (* EQ *);
  269 (* GE *);
  270 (* GT *);
  271 (* LE *);
  272 (* LT *);
  273 (* NE *);
  274 (* BLAND *);
  275 (* BLOR *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LBRACE *);
  279 (* RBRACE *);
  280 (* COMMA *);
  281 (* SEMICOLON *);
  282 (* COLON *);
  283 (* QMARK *);
  284 (* IF *);
  285 (* THEN *);
  286 (* ELSE *);
  287 (* WHILE *);
  288 (* FOR *);
  289 (* RETURN *);
  290 (* AND *);
  291 (* ARROW *);
  292 (* FUN *);
  293 (* IN *);
  294 (* LET *);
  295 (* REC *);
  296 (* TYPE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* TP *);
  259 (* BCONSTANT *);
  260 (* INTCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\005\000\006\000\006\000\006\000\006\000\008\000\
\008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\011\000\011\000\011\000\011\000\
\007\000\012\000\012\000\004\000\004\000\013\000\003\000\003\000\
\016\000\015\000\015\000\015\000\014\000\014\000\000\000"

let yylen = "\002\000\
\001\000\004\000\002\000\001\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\003\000\001\000\001\000\001\000\003\000\
\001\000\001\000\002\000\001\000\006\000\008\000\002\000\003\000\
\005\000\001\000\003\000\003\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\001\000\000\000\000\000\000\000\
\021\000\022\000\023\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\028\000\000\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\003\000\032\000\000\000\000\000\000\000\024\000\000\000\
\002\000\005\000\006\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\020\000\000\000\000\000\
\038\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\033\000\035\000\030\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\014\000\023\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\007\000\045\000\070\000\046\000"

let yysindex = "\011\000\
\238\254\000\000\239\254\000\000\000\000\001\255\002\255\036\255\
\000\000\000\000\000\000\001\255\001\255\002\255\044\255\000\000\
\027\255\246\255\000\000\010\255\000\000\026\255\238\254\077\255\
\074\255\069\255\099\000\010\255\010\255\010\255\010\255\010\255\
\010\255\010\255\010\255\010\255\010\255\010\255\010\255\010\255\
\000\000\000\000\000\000\102\255\088\255\077\255\000\000\001\255\
\000\000\000\000\000\000\000\000\044\255\044\255\027\255\027\255\
\027\255\027\255\027\255\027\255\000\000\000\000\090\255\008\255\
\000\000\087\255\008\255\000\000\008\255\247\254\001\255\252\254\
\253\254\001\255\008\255\000\000\000\000\000\000\000\000\083\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\255\000\000\
\113\255\250\254\000\000\031\255\000\000\000\000\005\255\095\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\095\255\000\000\000\000\
\000\000\000\000\000\000\000\000\075\255\094\255\132\255\151\255\
\170\255\189\255\208\255\227\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\251\254"

let yygindex = "\000\000\
\000\000\000\000\097\000\244\255\108\000\026\000\037\000\233\000\
\000\000\040\000\000\000\113\000\000\000\089\000\009\000\000\000"

let yytablesize = 271
let yytable = "\025\000\
\026\000\009\000\074\000\010\000\011\000\031\000\036\000\031\000\
\031\000\068\000\009\000\001\000\010\000\011\000\018\000\036\000\
\077\000\078\000\018\000\003\000\012\000\008\000\018\000\018\000\
\031\000\075\000\022\000\069\000\013\000\012\000\075\000\075\000\
\031\000\031\000\032\000\066\000\024\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\042\000\026\000\028\000\029\000\030\000\026\000\
\053\000\054\000\076\000\026\000\026\000\079\000\008\000\008\000\
\050\000\051\000\052\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\072\000\008\000\073\000\061\000\062\000\
\008\000\009\000\009\000\080\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\047\000\009\000\
\044\000\048\000\049\000\009\000\010\000\010\000\063\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\064\000\010\000\067\000\071\000\075\000\010\000\043\000\
\037\000\027\000\010\000\010\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\041\000\011\000\065\000\000\000\
\000\000\011\000\000\000\000\000\000\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000\
\012\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\013\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\013\000\013\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\000\000\014\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\014\000\014\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\000\000\015\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\015\000\015\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\000\000\016\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\016\000\016\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\000\000\017\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\017\000\
\017\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\055\000\056\000\057\000\058\000\059\000\060\000"

let yycheck = "\012\000\
\013\000\001\001\012\001\003\001\004\001\001\001\012\001\003\001\
\004\001\002\001\001\001\001\000\003\001\004\001\021\001\021\001\
\021\001\021\001\025\001\038\001\020\001\039\001\029\001\030\001\
\020\001\035\001\025\001\020\001\028\001\020\001\035\001\035\001\
\028\001\007\001\008\001\048\000\001\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\025\001\021\001\009\001\010\001\011\001\025\001\
\031\000\032\000\071\000\029\001\030\001\074\000\007\001\008\001\
\028\000\029\000\030\000\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\067\000\021\001\069\000\039\000\040\000\
\025\001\007\001\008\001\075\000\029\001\030\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\021\001\021\001\
\020\001\029\001\000\000\025\001\007\001\008\001\001\001\029\001\
\030\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\026\001\021\001\026\001\030\001\035\001\025\001\023\000\
\026\001\014\000\029\001\030\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\000\021\001\046\000\255\255\
\255\255\025\001\255\255\255\255\255\255\029\001\030\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\255\255\
\021\001\255\255\255\255\255\255\025\001\255\255\255\255\255\255\
\029\001\030\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\021\001\255\255\255\255\255\255\025\001\
\255\255\255\255\255\255\029\001\030\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\021\001\255\255\
\255\255\255\255\025\001\255\255\255\255\255\255\029\001\030\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\021\001\255\255\255\255\255\255\025\001\255\255\255\255\
\255\255\029\001\030\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\255\255\021\001\255\255\255\255\255\255\
\025\001\255\255\255\255\255\255\029\001\030\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\021\001\
\255\255\255\255\255\255\025\001\255\255\255\255\255\255\029\001\
\030\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\033\000\034\000\035\000\036\000\037\000\038\000"

let yynames_const = "\
  FST\000\
  SND\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  EQ\000\
  GE\000\
  GT\000\
  LE\000\
  LT\000\
  NE\000\
  BLAND\000\
  BLOR\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  AND\000\
  ARROW\000\
  FUN\000\
  IN\000\
  LET\000\
  REC\000\
  TYPE\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  TP\000\
  BCONSTANT\000\
  INTCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 45 "parser.mly"
            ( _1 )
# 301 "parser.ml"
               : Lang.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fundefn_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'mlexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'end_marker) in
    Obj.repr(
# 50 "parser.mly"
  (Prog (_1, _2) )
# 310 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
  ( )
# 316 "parser.ml"
               : 'end_marker))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_or_call) in
    Obj.repr(
# 60 "parser.mly"
    ( _1 )
# 323 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_or_call) in
    Obj.repr(
# 62 "parser.mly"
     ( binary_exp _1 MUL _3 )
# 331 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_or_call) in
    Obj.repr(
# 64 "parser.mly"
     ( binary_exp _1 DIV _3 )
# 339 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_or_call) in
    Obj.repr(
# 66 "parser.mly"
     ( binary_exp _1 MOD _3 )
# 347 "parser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 71 "parser.mly"
    ( _1 )
# 354 "parser.ml"
               : 'additive_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 73 "parser.mly"
   ( binary_exp _1 ADD _3 )
# 362 "parser.ml"
               : 'additive_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 75 "parser.mly"
   ( binary_exp _1 SUB _3 )
# 370 "parser.ml"
               : 'additive_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 80 "parser.mly"
    ( _1 )
# 377 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 82 "parser.mly"
   ( binary_exp _1 EQ _3 )
# 385 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 84 "parser.mly"
   ( binary_exp _1 GE _3 )
# 393 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 86 "parser.mly"
   ( binary_exp _1 GT _3 )
# 401 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 88 "parser.mly"
   ( binary_exp _1 LE _3 )
# 409 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 90 "parser.mly"
   ( binary_exp _1 LT _3 )
# 417 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_exp) in
    Obj.repr(
# 92 "parser.mly"
   ( binary_exp _1 NE _3 )
# 425 "parser.ml"
               : 'compar_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compar_exp) in
    Obj.repr(
# 99 "parser.mly"
    ( _1 )
# 432 "parser.ml"
               : 'bool_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_exp) in
    Obj.repr(
# 101 "parser.mly"
    ( binary_exp _1 BLAND _3 )
# 440 "parser.ml"
               : 'bool_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compar_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_exp) in
    Obj.repr(
# 103 "parser.mly"
    ( binary_exp _1 BLOR _3 )
# 448 "parser.ml"
               : 'bool_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
    ( VarE(_1) )
# 455 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 111 "parser.mly"
     ( Const(BoolV(_1)) )
# 462 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
     ( Const(IntV(_1)) )
# 469 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mlexp) in
    Obj.repr(
# 115 "parser.mly"
     ( _2 )
# 476 "parser.ml"
               : 'primary_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list) in
    Obj.repr(
# 120 "parser.mly"
    ( match _1 with
    | [e] -> e
    | es -> CallE(es)
    )
# 486 "parser.ml"
               : 'primary_exp_or_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp) in
    Obj.repr(
# 128 "parser.mly"
    ( [_1] )
# 493 "parser.ml"
               : 'primary_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'primary_exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary_exp_list) in
    Obj.repr(
# 130 "parser.mly"
    ( _1 :: _2 )
# 501 "parser.ml"
               : 'primary_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_exp) in
    Obj.repr(
# 135 "parser.mly"
    ( _1 )
# 508 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'mlexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mlexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 137 "parser.mly"
    ( IfThenElse(_2, _4, _6) )
# 517 "parser.ml"
               : 'mlexp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'paramdecl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'complex_tp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'mlexp) in
    Obj.repr(
# 158 "parser.mly"
  ( Fundefn (FPdecl (_6, _3, _4), _8) )
# 527 "parser.ml"
               : 'fundefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fundefn) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'end_marker) in
    Obj.repr(
# 163 "parser.mly"
  ( [_1] )
# 535 "parser.ml"
               : 'fundefn_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fundefn) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'end_marker) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fundefn_list) in
    Obj.repr(
# 165 "parser.mly"
    ( _1 :: _3 )
# 544 "parser.ml"
               : 'fundefn_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'complex_tp) in
    Obj.repr(
# 170 "parser.mly"
  ( Vardecl(_2, _4) )
# 552 "parser.ml"
               : 'paramdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lang.tp) in
    Obj.repr(
# 175 "parser.mly"
  ( _1 )
# 559 "parser.ml"
               : 'complex_tp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'complex_tp) in
    Obj.repr(
# 177 "parser.mly"
  ( _2 )
# 566 "parser.ml"
               : 'complex_tp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'complex_tp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'complex_tp) in
    Obj.repr(
# 179 "parser.mly"
      ( FunT(_1, _3) )
# 574 "parser.ml"
               : 'complex_tp))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "parser.mly"
  ( [] )
# 580 "parser.ml"
               : 'paramdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'paramdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecl_list) in
    Obj.repr(
# 186 "parser.mly"
    ( _1 :: _2 )
# 588 "parser.ml"
               : 'paramdecl_list))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lang.prog)
