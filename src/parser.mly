%{

let mark data start_pos end_pos =
  Mark.create data start_pos end_pos 

%}

%token Eof
%token LParen RParen Comma
%token True False
%token <int> Int_literal
%token <Symbol.t> Ident
%token Fun Let Equal Arrow In Split As

%type <Ast.mexp> program

%start program

%%

m(x) :
  | x = x;
      { mark x $startpos(x) $endpos(x) }
  ;

program : e = m(exp); Eof { e }

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
