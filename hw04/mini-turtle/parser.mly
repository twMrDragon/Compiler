
/* Parsing for mini-Turtle */

%{
  open Ast

%}

/* Declaration of tokens */

%token EOF
/* To be completed */
%token <int> CST
%token <string> IDENT
%token FORWARD
%token PLUS MINUS TIMES DIV
%token LP RP
%token PENUP PENDOWN
%token TURNLEFT TURNRIGHT
%token COLOR
%token BLACK WHITE RED GREEN BLUE
%token IF ELSE
%token REPEAT
%token BEGIN END
%token DEF
%token COMMA

/* Priorities and associativity of tokens */
%left PLUS MINUS
%left TIMES DIV
%nonassoc unary_minus

/* To be completed */

/* Axiom of the grammar */
%start prog

/* Type of values ​​returned by the parser */
%type <Ast.program> prog

%%

/* Production rules of the grammar */

prog:
  defs = def*
  main = stmt*
  /* To be completed */ EOF
    { { defs = defs; main = Sblock main } (* To be modified *) }
;

def:
  | DEF f = IDENT LP formals  = separated_list(COMMA, IDENT) RP ss=stmt
    {{
      name = f;
      formals=formals;
      body = ss
    }}


expr:
  | c = CST
    {Econst c}
  | e1 = expr o = binop e2 = expr
    {Ebinop (o, e1, e2)}
  | LP e = expr RP
    {e}
  | MINUS e = expr %prec unary_minus
    {Ebinop(Sub, Econst 0, e)} 
  | id = IDENT {Evar id}
;

stmt:
  | FORWARD e = expr
    {Sforward e}
  | PENUP
    {Spenup}
  | PENDOWN
    {Spendown}
  | TURNLEFT e = expr
    {Sturn e}
  | TURNRIGHT e = expr
    {Sturn (Ebinop(Sub, Econst 0, e))}
  | COLOR c = color 
    {Scolor c}
  | IF e = expr s = stmt
    {Sif (e,s,Sblock [])}
  | IF e=expr s1=stmt ELSE s2=stmt
    {Sif (e,s1,s2)}
  | REPEAT e=expr s=stmt
    {Srepeat(e,s)}
  | BEGIN ss = stmt* END
    {Sblock ss}
  | id=IDENT LP ss = separated_list(COMMA,expr) RP
    {Scall (id,ss)}
;

%inline binop:
| PLUS {Add}
| MINUS {Sub}
| TIMES {Mul}
| DIV {Div}
;

color:
  | BLACK { Turtle.black }
  | WHITE { Turtle.white }
  | RED   { Turtle.red   }
  | GREEN { Turtle.green }
  | BLUE  { Turtle.blue  }
;