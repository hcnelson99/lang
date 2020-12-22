%{

let mark data start_pos end_pos =
  Mark.create data start_pos end_pos 

%}

%token Eof
%token LParen RParen
%token <int> Int_const
%token <Symbol.t> Ident
%token Fun Let Equal Arrow In

%type <Ast.mexp> program

%start program

%%

m(x) :
  | x = x;
      { mark x $startpos(x) $endpos(x) }
  ;

program : e = m(exp); Eof { e }

exp_atom : 
  | i = m(Ident) { Ast.Var (Mark.obj i) }
  | i = m(Int_const) { Ast.Int_const (Mark.obj i) }
  | LParen; e = exp; RParen; { e }

exp_ap :
  | e = exp_atom; { e }
  | e1 = m(exp_ap); e2 = m(exp_atom); { Ast.Ap (e1, e2) }

exp_fun :
  | e = exp_ap; { e }
  | Fun; i = m(Ident); Arrow; e = m(exp_fun); { Ast.Abs (i, e) }


exp : 
  | e = exp_fun; { e }
  | Let; i = m(Ident); Equal; e1 = m(exp); In; e2 = m(exp); { Ast.Let (i, e1, e2) }

