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

atom : 
  | i = m(Ident) { Ast.Var (Mark.obj i) }
  | i = m(Int_const) { Ast.Int_const (Mark.obj i) }
  (* | Let; i = m(Ident); Equal; e1 = m(exp); In; e2 = m(exp); { Ast.Let (i, e1, e2) } *)
  | LParen; e = exp; RParen; { e }

exp : 
  | e = atom; { e }
  | Fun; i = m(Ident); Arrow; e = m(exp); { Ast.Abs (i, e) }
  | e1 = m(exp); e2 = m(atom); { Ast.Ap (e1, e2) }

