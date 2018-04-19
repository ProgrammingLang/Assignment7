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
"-"                             { return 'MINUS'; }
"add1"                          { return 'ADD1'; }
"from"                          { return 'FROM'; }
","                   		      { return 'COMMA'; }
"~"                             { return 'NEGATIVE'; }
"=>"                   		      { return 'THATRETURNS'; }
"==="                           { return 'BOOLEQ'; }
"="								{ return 'EQ'; }
"["                             { return 'LBRACKET'; }
"]"                             { return 'RBRACKET'; }
"/"                             { return 'DIVIDE'; }
"%"                             { return 'MOD'; }
"::"							{ return 'CONS'; }
">"                             { return 'GT'; }
"<"                             { return 'LT'; }
"let"							{ return 'LET'; }
"in"							{ return 'IN'; }
"end"							{ return 'END'; }
"hd"							{ return 'HD'; }
"tl" 							{ return 'TL'; }
"sumlist"                       { return 'SUMLIST'; }
"not"                           { return 'NOT'; }
"isNull"						{ return 'ISNULL'; }
"if"                            { return 'IF'; }
"then"                          { return 'THEN'; }
"else"                          { return 'ELSE'; }
<<EOF>>               		      { return 'EOF'; }
{LETTER}({LETTER}|{DIGIT}|_)*  	      { return 'VAR'; }
{DIGIT}+                              { return 'INT'; }
.                     		      { return 'INVALID'; }




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
    | if_exp        { $$ = $1; }
	| let_exp		{ $$ = $1; }
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
    | NOT LPAREN exp RPAREN
        { $$ = SLang.absyn.createPrim1AppExp("not", $3); }
	| HD LPAREN list_exp RPAREN
		{ $$ = SLang.absyn.createPrim1AppExp("hd",$3); }
	| TL LPAREN list_exp RPAREN
		{ $$ = SLang.absyn.createPrim1AppExp("tl",$3); }
	| ISNULL LPAREN list_exp RPAREN
		{ $$ = SLang.absyn.createPrim1AppExp("isNull",$3); }
    ;

prim2_app_exp
    : LPAREN exp prim2_op exp RPAREN
       { $$ = SLang.absyn.createPrim2AppExp($3,$2,$4); }
	| LPAREN exp CONS list_exp RPAREN
		{ $$ = SLang.absyn.createPrim2AppExp("::",$2,$4); }
    ;

prim1_op
    :  ADD1     { $$ = $1; }
    |  NEGATIVE { $$ = $1; }
    ;

prim2_op
    :  PLUS     { $$ = $1; }
    |  TIMES    { $$ = $1; }
    |  MINUS    { $$ = $1; }
    |  MOD      { $$ = $1; }
    |  DIVIDE   { $$ = $1; }
    |  BOOLEQ   { $$ = $1; }
    |  GT       { $$ = $1; }
    |  LT       { $$ = $1; }
    ;

if_exp
    :  IF exp THEN exp ELSE exp
        { $$ = SLang.absyn.createIfExp("if",$2,$4,$6); }
    ;

let_exp
	: LET bindings IN exp END
		{   $2[1].unshift("args");
			$$ = SLang.absyn.createAppExp(SLang.absyn.createFnExp($2[0],$4), $2[1]); }
	;

bindings 
	: VAR EQ exp 
	{ 	var a = [$1];
		var b = [$3];
		$$ = [a, b]; }
	| VAR EQ exp bindings
	{	var a = $4[0].push($1);
		var b = $4[1].push($3); 
		$$ = $4; }
		
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
