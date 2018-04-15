/* description: Grammar for SLang 1 */

/* lexical grammar */
%lex

DIGIT		      [0-9]
LETTER		      [a-zA-Z]

%%

\s+                                   { /* skip whitespace */ }
"fn"				      { return 'FN'; }
"("                   		      { return 'LPAREN'; }
")"                   		      { return 'RPAREN'; }
"+"                   		      { return 'PLUS'; }
"*"                   		      { return 'TIMES'; }
"subtract"                             { return 'MINUS'; }
"add1"                                { return 'ADD1'; }
"from"                          { return 'FROM'; }
","                   		      { return 'COMMA'; }
"=>"                   		      { return 'THATRETURNS'; }
"["                                   { return 'LBRACKET'; }
"]"                                   { return 'RBRACKET'; }
<<EOF>>               		      { return 'EOF'; }
{LETTER}({LETTER}|{DIGIT}|_)*  	      { return 'VAR'; }
{DIGIT}+                              { return 'INT'; }
.                     		      { return 'INVALID'; }
"sumlist"                       { return 'SUMLIST'; }


/lex

%start program

%% /* language grammar */

program
    : exp EOF
        { return SLang.absyn.createProgram($1); }
    ;

exp
    : var_exp       { $$ = $1; }
    | intlit_exp    { $$ = $1; }
    | fn_exp        { $$ = $1; }
    | app_exp       { $$ = $1; }
    | prim1_app_exp { $$ = $1; }
    | prim2_app_exp { $$ = $1; }
    | list_exp      { $$ = $1; }
    ;

var_exp
    : VAR  { $$ = SLang.absyn.createVarExp( $1 ); }
    ;

intlit_exp
    : INT  { $$ =SLang.absyn.createIntExp( $1 ); }
    ;

fn_exp
    : FN LPAREN formals RPAREN THATRETURNS exp
           { $$ = SLang.absyn.createFnExp($3,$6); }
    ;

formals
    : /* empty */ { $$ = [ ]; }
    | VAR moreformals
        { var result;
          if ($2 === [ ])
	     result = [ $1 ];
          else {
             $2.unshift($1);
             result = $2;
          }
          $$ = result;
        }
    ;

moreformals
    : /* empty */ { $$ = [ ] }
    | COMMA VAR moreformals
       { $3.unshift($2);
         $$ = $3; }
    ;

app_exp
    : LPAREN exp args RPAREN
       {  $3.unshift("args");
          $$ = SLang.absyn.createAppExp($2,$3); }
    ;

prim1_app_exp
    : prim1_op LPAREN exp RPAREN
       { $$ = SLang.absyn.createPrim1AppExp($1,$3); }
    | SUMLIST LPAREN list_exp RPAREN
        { $$ = SLang.absyn.createPrim1AppExp("+", $3); }
    ;

prim2_app_exp
    : prim2_op LPAREN exp COMMA exp RPAREN
       { $$ = SLang.absyn.createPrim2AppExp($1,$3,$5); }
    | MINUS exp FROM exp
        { $$ = SLang.absyn.createPrim2AppExp("-",$2,$4); }
    ;

prim1_op
    :  ADD1     { $$ = $1; }
    ;

prim2_op
    :  PLUS     { $$ = $1; }
    |  TIMES    { $$ = $1; }
    ;

args
    : /* empty */ { $$ = [ ]; }
    | exp args
        { var result;
          if ($2 === [ ])
	     result = [ $1 ];
          else {
             $2.unshift($1);
             result = $2;
          }
          $$ = result;
        }
    ;

prim_args
    :  /* empty */ { $$ = [ ]; }
    |  exp more_prim_args    { $2.unshift($1); $$ = $2; }
    ;

more_prim_args
    : /* empty */ { $$ = [ ] }
    | COMMA exp more_prim_args { $3.unshift($2); $$ = $3; }
    ;

list_exp
    : LBRACKET int_list RBRACKET { $$ = SLang.absyn.createListExp($2); }
    ;

int_list
    : /* empty */        { $$ = []; }
    | INT more_ints      { $2.unshift(parseInt($1)); $$ = $2; }
    ;

more_ints
    : /* empty */         { $$ = []; }
    | COMMA INT more_ints { $3.unshift(parseInt($2)); $$ = $3;}
    ;
%%