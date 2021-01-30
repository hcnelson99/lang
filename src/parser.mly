%{

let mark data start_pos end_pos =
  Mark.create data start_pos end_pos 

%}

%token Eof
%token LParen RParen Comma
%token True False Bool Int Star Unit
%token <int> Int_literal
%token <Symbol.t> Ident
%token Type Of Bar 
(* %token Match With *)
%token Fun Let End Equal Arrow In Split As

%type <Ast.mstmt list> program
%start program

%%

m(x) :
  | x = x;
      { mark x $startpos(x) $endpos(x) }
  ;

exp_tuple :
  | (* empty *) { [] }
  | Comma; e = m(exp); tpl = exp_tuple { e :: tpl } 

exp_atom : 
  | i = m(Ident) { Ast.Var (Mark.obj i) }
  | i = m(Int_literal) { Ast.Int (Mark.obj i) }
  | True; { Ast.Bool true } 
  | False; { Ast.Bool false } 
  | LParen;  RParen; { Ast.Tuple [] }
  | LParen; e = m(exp); tpl = exp_tuple; RParen; { 
      match tpl with
      | [] -> Mark.obj e 
      | _ -> Ast.Tuple (e :: tpl)
  }

exp_ap :
  | e = exp_atom; { e }
  | e1 = m(exp_ap); e2 = m(exp_atom); { Ast.Ap (e1, e2) }

exp_fun :
  | e = exp_ap; { e }
  | Fun; i = m(Ident); Arrow; e = m(exp); { Ast.Abs (i, e) }

split_tuple_list : 
  | i = m(Ident) { [i] }
  | i = m(Ident); Comma; is = split_tuple_list; { i::is }

exp : 
  | e = exp_fun; { e }
  | Let; i = m(Ident); Equal; e1 = m(exp); In; e2 = m(exp); { Ast.Let (i, e1, e2) }
  | Split; e1 = m(exp); As; LParen; is = split_tuple_list; RParen; In; e2 = m(exp); { Ast.Split (e1, is, e2) }


ty_atom: 
  | Int; { Ast.Int }
  | Bool; { Ast.Bool }
  | Unit; { Ast.Prod [] }
  | LParen; t = ty; RParen; { t } 

prod_tail :
  | t = m(ty_atom); { [t] }
  | t = m(ty_atom); Star; ts = prod_tail; { t::ts }

ty_prod :
  | t = ty_atom; { t } 
  | t = m(ty_atom); Star; ts = prod_tail; { Ast.Prod (t::ts) }

ty :
  | t = ty_prod; { t } 
  | t1 = m(ty_prod); Arrow; t2 = m(ty); { Ast.Arrow (t1, t2) }

con_decl : 
  | c = m(Ident); { (c, None) } 
  | c = m(Ident); Of; t = m(ty) { (c, Some t) } 

con_decls :
  |  (* empty *) { [] }
  | Bar; c = con_decl; cs = con_decls; { c :: cs }

stmt :
  | Let; i = m(Ident); Equal; e = m(exp); End; { Ast.LetStmt (i, e) }
  | Type; i = m(Ident); Equal; c = con_decls; { Ast.TypeDecl (i, c) }

stmts : 
  | (* empty *) { [] }
  | s = m(stmt); ss = stmts; { s :: ss }

program : ss = stmts; Eof { ss }
