%{

let mark data start_pos end_pos =
  Mark.create data start_pos end_pos 

%}

%token Eof
%token LParen
%token RParen
%token <Int32.t> Int_const
%token <Symbol.t> Ident
%token Plus
%token Fun

%type <Ast.mexp> program

%start program

%%

m(x) :
  | x = x;
      { mark x $startpos(x) $endpos(x) }
  ;

program : e = exp; Eof { e }

exp : 
  | i = m(Ident) { Mark.with_mark i (Ast.Var (Mark.obj i)) }

