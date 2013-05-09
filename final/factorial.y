%{
/* Para testar apenas o lex, deve ter um yacc mínimo para gerar o y.tab.h
   Basta definir os tokens necessários à linguagem a testar (%token).
   Também a rotina main() deve chamar apenas o lex (yylex()).
   Devemos igualmente fornecer uma rotina de impressão de erros yyerror.
   A gramática é vazia (file: ;) pelo que só reconhece a linguagem vazia, não devendo ser chamado o analisador sintáctico do yacc (yyparse()).

Compilar com:
byacc -dv solex.y
flex -dl lang.l
gcc lex.yy.c y.tab.c

Executar os exemplos (apenas com redirecção):
./lang < exemplo.lang

Para garantir que as expressões regulares reconhecem correctamente as sequências de entrada deve adicionar o modo debug (-d) ao flex.
Ao executar os diversos exemplos deve verificar quais as expressões regulares que as reconhecem cada uma das sequências de entrada.
As expressões regulares são identificadas pelo número da linha em que se encontram no ficheiro lex.
*/   
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h> 
#include <stdarg.h>
#include "node.h"
#include "tabid.h"

extern void program(Node *body); 

%}

%union {
	int i;			/* integer value */
	char *s;		/* symbol name or string literal */
	double d;		/* number value */
	Node *n;
};

%token <i> INTEGER 
%token <s> STRING IDENT
%token <d> NUMBER
%token VOID VINT VSTR PUBLIC VNUMB CONST IF THEN ELSE WHILE
%token DO FOR IN STEP UPTO DOWNTO BREAK CONTINUE IDENT STRING
%token ROOT FACEXP NOTEXP ADDREXP VALEXP BINOPSUM BINOPMIN BINOPMUL BINOPDIV BINOPREM BINOPLESS BINOPGREAT BINOPEQ BINOPOR BINOPAND BINOPEQ BINOPGEQ BINOPLEQ BINOPNEQ EXPRESSION
%token DECLS DECL DECL_SPEC PUBLIC_CONST END IDENT_PTR INIT_DECL FUNC_PARAM PARAM BODY BODY

%nonassoc IFX '(' ')' '[' ']' PP MM '~' '!'
%nonassoc ELSE

%right ASG
%left '|' '&' 
%left GE LE NE '>' '<' '='
%left '+' '-'
%left '*' '/' '%' 
%nonassoc UMINUS

%type <n> entry_point declaration type_specifier keywords_specifiers init_declarator initializer func_parameters func_parameters_aux func_invoc_param func_invoc_param_aux left_value expression declarator declaration_specifiers body body_contents parameter parameters statement

%%
file			: entry_point					{ program(uniNode(ROOT, $1)); }
			| /* empty file */
			;

entry_point		: declaration					{ $$ = $1; }
			| entry_point declaration			{ $$ = subNode(DECLS, 2, $1, $2); }
			;

declaration		: declaration_specifiers ';'			{ $$ = subNode(DECL, 1, $1); }
			| declaration_specifiers init_declarator ';'	{ $$ = subNode(DECL, 2, $1, $2); }
			;

declaration_specifiers	: keywords_specifiers type_specifier declarator	{ $$ = subNode(DECL_SPEC, 3, $1, $2, $3); } 
			;

keywords_specifiers	: PUBLIC					{ $$ = nilNode(PUBLIC); }
			| PUBLIC CONST					{ $$ = nilNode(PUBLIC_CONST); }
			| CONST						{ $$ = nilNode(CONST); }
			| /* no specifiers */				{ $$ = nilNode(END); }
			;

type_specifier		: VINT						{ $$ = nilNode(VINT); }
			| VSTR						{ $$ = nilNode(VSTR); }
			| VNUMB						{ $$ = nilNode(VNUMB); }
			| VOID						{ $$ = nilNode(VOID); }
			;
	
init_declarator		: ASG initializer				{ }
			| ASG IDENT					{ }
			| '(' func_parameters ')' body			{ $$ = subNode(INIT_DECL, 2, $2, $4); }
			;

declarator		: '*' IDENT					{ $$ = strNode(IDENT_PTR, $2); }
			| IDENT						{ $$ = strNode(IDENT, $1); }
			;

initializer		: INTEGER					{ }
			| STRING					{ }
			| NUMBER					{ }
			;

body			: /* no body */					{ $$ = nilNode(END); }
			| '{' body_contents '}'				{ $$ = $2; }
			;

body_contents		: /* no contents */				{ $$ = nilNode(END); }
			| body_contents parameters			{ $$ = subNode(BODY, 1, $2); }
			| body_contents statement			{ $$ = subNode(BODY, 1, $2); }
			;

func_parameters		: func_parameters_aux				{ $$ = $1; }
			| /* no parameters */				{ $$ = nilNode(END); }
			;

func_parameters_aux	: parameter					{ $$ = subNode(FUNC_PARAM, 1, $1); }
			| parameter ',' func_parameters_aux		{ $$ = subNode(FUNC_PARAM, 2, $1, $3); }
			;

parameters		: parameter ';'
			| parameter ',' parameters
			;

parameter		: type_specifier declarator			{ $$ = subNode(PARAM, 2, $1, $2); }
			;

statement		: selection_statement
			| iteration_statement
			| expression ';'
			| statement_body
			| jump_statement
			| left_value '#' expression ';'
			| ';'
			;

statement_body		: '{' 						{ } 
			  body_contents '}'				{ }
			;

left_value		: IDENT						{ }
			| IDENT '[' expression ']'			{ }
			;

selection_statement	: IF expression THEN statement %prec IFX
			| IF expression THEN statement ELSE statement
			;


iteration_statement	: DO statement WHILE expression ';'
			| FOR left_value IN expression for_cond expression for_step DO statement
			;

jump_statement		: BREAK INTEGER ';'
			| BREAK ';'
			| CONTINUE INTEGER ';'
			| CONTINUE ';'
			;

for_cond		: UPTO
			| DOWNTO
			;

for_step		: STEP expression
			|
			;

func_invoc_param	: func_invoc_param_aux				{ }
			| /* no args */					{ }
			;

func_invoc_param_aux	: expression					{ }
			| expression ',' func_invoc_param_aux		{ }
			;	

expression		: left_value 					{ }
	    		| left_value ASG expression			{ }
			| IDENT 					{ } 
			  '(' func_invoc_param ')'			{ }
			| PP left_value 				{ } 
			| MM left_value					{ }
			| left_value PP					{ }
			| left_value MM					{ }
			| initializer					{ }
			| '-' expression %prec UMINUS			{ }
			| expression '!'				{ $$ = uniNode(FACEXP, $1); }
			| '~' expression				{ $$ = uniNode(NOTEXP, $2); }
			| '*' expression				{ }
			| '&' left_value				{ }
			| expression '+' expression			{ $$ = binNode(BINOPSUM, $1, $3); }
			| expression '-' expression			{ $$ = binNode(BINOPMIN, $1, $3); }
			| expression '*' expression			{ $$ = binNode(BINOPMUL, $1, $3); }
			| expression '/' expression			{ $$ = binNode(BINOPDIV, $1, $3); }
			| expression '%' expression			{ $$ = binNode(BINOPREM, $1, $3); }
			| expression '<' expression			{ $$ = binNode(BINOPLESS, $1, $3); }
			| expression '>' expression			{ $$ = binNode(BINOPGREAT, $1, $3); }
			| expression '=' expression			{ $$ = binNode(BINOPEQ, $1, $3); }
			| expression '|' expression			{ $$ = binNode(BINOPOR, $1, $3); }
			| expression '&' expression			{ $$ = binNode(BINOPAND, $1, $3); }
			| expression GE expression			{ $$ = binNode(BINOPGEQ, $1, $3); }
			| expression LE expression			{ $$ = binNode(BINOPLEQ, $1, $3); }
			| expression NE expression			{ $$ = binNode(BINOPNEQ, $1, $3); }
			| '(' expression ')'				{ $$ = uniNode(EXPRESSION, $2); }
			;

%%

char **yynames =
#if YYDEBUG > 0
	(char**) yyname;
#else
	0;
#endif
