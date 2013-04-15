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
#include "tabid.h"

#define TRUE 1
#define FALSE 0
#define TINT 0
#define TSTR 1
#define TNUMB 2
#define TVOID 3
#define TFUNC 32

#define ERR_MISTYPES "Mismatched types"
#define ERR_DEFVOID "Cannot define void variables"
#define ERR_INITVOID "Cannot initialize void variables"
#define ERR_VARNOTDEF "Variable not defined"
#define ERR_NOTFUNC "Identifier is not a function"
#define ERR_WRONGNARGS "Wrong number of arguments in function call"
#define ERR_INDEXTYPE "Index type is not integer"
#define ERR_OPTYPE "Operation not allowed on given type"

#define concSize6Bit(X) (X & 63)
#define getType(X) (X & 3)
#define getPtr(X) (X & 4)
#define getFunc(X) (X & 32)

#define setInt(X) (X | 0)
#define setStr(X) (X | 1)
#define setNumb(X) (X | 2)
#define setVoid(X) (X | 3)
#define setPtr(X) (X | 4)
#define setPub(X) (X | 8)
#define setConst(X) (X | 16)
#define setFunc(X) (X | 32)

int buildIdent(int keywords, int type, int isPtr, char *ident);
int convertTypes(int type);
int cmpTypes(int a, int b);
void mkcall(char *func, long eargs);
int cmpTypesList(int src, int size,  ...);
%}

%union {
	int i;			/* integer value */
	char *s;		/* symbol name or string literal */
	double d;		/* number value */
};

%token <i> INTEGER
%token <s> STRING IDENT
%token <d> NUMBER
%token VOID VINT VSTR PUBLIC VNUMB CONST IF THEN ELSE WHILE
%token DO FOR IN STEP UPTO DOWNTO BREAK CONTINUE IDENT STRING
%token NONE POINTER

%nonassoc IFX '(' ')' '[' ']' PP MM '~' '!'
%nonassoc ELSE

%right ASG
%left '|' '&' 
%left GE LE NE '>' '<' '='
%left '+' '-'
%left '*' '/' '%' 
%nonassoc UMINUS

%type <i> type_specifier keywords_specifiers init_declarator initializer func_parameters func_parameters_aux func_invoc_param func_invoc_param_aux left_value expression
%type <s> declarator declaration_specifiers 

%%
file			: entry_point					{ IDprint(0, -1); }
			| /* empty file */
			;

entry_point		: declaration
			| entry_point declaration
			;

declaration		: declaration_specifiers ';'			{ if (getType(IDfind($1/*ident*/, (long*)IDtest)) == TVOID) { yyerror(ERR_DEFVOID);  } }
			| declaration_specifiers init_declarator ';'
			;

declaration_specifiers	: keywords_specifiers type_specifier declarator	{ $$ = $3; } 
			;

keywords_specifiers	: PUBLIC					{ $$ = PUBLIC; }
			| PUBLIC CONST					{ $$ = PUBLIC + CONST; }
			| CONST						{ $$ = CONST; }
			| /* no specifiers */				{ $$ = NONE; }
			;

type_specifier		: VINT						{ $$ = VINT; }
			| VSTR						{ $$ = VSTR; }
			| VNUMB						{ $$ = VNUMB; }
			| VOID						{ $$ = VOID; }
			;
	
init_declarator		: ASG initializer				{ int type = IDfind($<s>0/*ident*/, (long*)IDtest);
									  if (cmpTypes(type, convertTypes($2)/*type*/)) { yyerror(ERR_MISTYPES); } 
									  if (getType(type) == TVOID) { yyerror(ERR_INITVOID);  } }
			| ASG IDENT					{ if (cmpTypes(IDfind($<s>0/*ident*/,(long*)IDtest),IDfind($2, (long*)IDtest)/*type*/)) { yyerror(ERR_MISTYPES); } }
			| '(' 						{ IDreplace(setFunc(IDfind($<s>0/*ident*/, (long*)IDtest)), $<s>0/*ident*/, 0); IDpush() ;} 
			  func_parameters 				{ IDreplace(IDfind($<s>0/*ident*/, (long*)IDtest), $<s>0/*ident*/, (long)$3/*#param*/); }
			  ')' body					{ IDpop(); }
			;

declarator		: '*' IDENT					{ $$ = $2/*ident*/; buildIdent($<i>-1/*keywords*/, $<i>0/*type*/, TRUE /*pointer*/, $2/*ident*/); }
			| IDENT						{ $$ = $1/*ident*/; buildIdent($<i>-1/*keywords*/, $<i>0/*type*/, FALSE/*pointer*/, $1/*ident*/); }
			;

initializer		: INTEGER					{ $$ = INTEGER; }
			| STRING					{ $$ = STRING; }
			| NUMBER					{ $$ = NUMBER; }
			;

body			: /* no body */
			| '{' body_contents '}'
			;

body_contents		: /* no contents */
			| body_contents parameters
			| body_contents statement
			;

func_parameters		: func_parameters_aux				{ $$ = $1; }
			| /* no parameters */				{ $$ = 0; }
			;

func_parameters_aux	: parameter					{ $$ = 1; }
			| parameter ',' func_parameters_aux		{ $$ = $3 + 1; }
			;

parameters		: parameter ';'
			| parameter ',' parameters
			;

parameter		: type_specifier declarator			/* case where $<i>-1 in line #115 is unknown */
			;

statement		: selection_statement
			| iteration_statement
			| expression ';'
			| statement_body
			| jump_statement
			| left_value '#' expression ';'
			| ';'
			;

statement_body		: '{' 						{ IDpush(); } 
			  body_contents '}'				{ IDpop(); }
			;

left_value		: IDENT						{ if (($$ = IDfind($1/*ident*/, (long*)IDtest)) == -1) { yyerror(ERR_VARNOTDEF); } }
			| IDENT '[' expression ']'			{ if (($$ = IDfind($1/*ident*/, (long*)IDtest)) == -1) { yyerror(ERR_VARNOTDEF); }
									  if (cmpTypes($3/*type*/, TINT)) { yyerror(ERR_INDEXTYPE); } }
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

func_invoc_param	: func_invoc_param_aux				{ $$ = $1; }
			| /* no args */					{ $$ = 0; }
			;

func_invoc_param_aux	: expression					{ $$ = 1; }
			| expression ',' func_invoc_param_aux		{ $$ = $3 + 1; }
			;	

expression		: left_value 					{ $$ = $1/*type*/; }
	    		| left_value ASG expression			{ if (cmpTypes($1/*type*/, $3/*type*/)) { yyerror(ERR_MISTYPES); } $$ = $1/*type*/; }
			| IDENT '(' func_invoc_param ')'		{ mkcall($1/*ident*/, $3/*#nargs*/); $$ = getType(IDfind($1/*ident*/, (long*)IDtest)); }
			| PP left_value 				{ if (cmpTypes($2/*type*/, TINT)) { yyerror(ERR_OPTYPE); } $$ = TINT; } 
			| MM left_value					{ if (cmpTypes($2/*type*/, TINT)) { yyerror(ERR_OPTYPE); } $$ = TINT; }
			| left_value PP					{ if (cmpTypes($1/*type*/, TINT)) { yyerror(ERR_OPTYPE); } $$ = TINT; }
			| left_value MM					{ if (cmpTypes($1/*type*/, TINT)) { yyerror(ERR_OPTYPE); } $$ = TINT; }
			| initializer					{ $$ = convertTypes($1/*rawType*/); }
			| '-' expression %prec UMINUS			{ if (cmpTypesList($2/*type*/, 2/*listSize*/, TINT, TNUMB)) { yyerror(ERR_OPTYPE); } $$ = TINT; }
			| expression '!'				{ if (cmpTypes($1/*type*/, TINT)) { yyerror(ERR_OPTYPE); } $$ = TINT; }
			| '~' expression
			| '*' expression
			| '&' expression
			| expression '+' expression
			| expression '-' expression
			| expression '*' expression
			| expression '/' expression
			| expression '%' expression
			| expression '<' expression
			| expression '>' expression
			| expression '=' expression
			| expression '|' expression
			| expression '&' expression
			| expression GE expression
			| expression LE expression
			| expression NE expression
			| '(' expression ')'
			;

%%

int buildIdent(int keywords, int type, int isPtr, char *ident) {
	int var = 0;
	
	/* concatenate to 6Bit, keywords may be unknown/random due to some rules */
	switch (concSize6Bit(keywords)) {
		case PUBLIC: var = setPub(var); break;
		case CONST: var = setConst(var); break;
		case PUBLIC+CONST: var = setPub(setConst(var)); break;
	}

	switch (type) {
		case VINT: var = setInt(var); break;
		case VSTR: var = setStr(var); break;
		case VNUMB: var = setNumb(var); break;
		case VOID: var = setVoid(var); break;
	}

	if (isPtr) { var = setPtr(var); }
	
	IDnew(var, ident, 0);
	return var;
}

int convertTypes(int type) {
	switch (type) {
		case INTEGER: 
		case VINT:
			return TINT;
		case STRING: 
		case VSTR:
			return TSTR;
		case NUMBER:
		case VNUMB:
			return TNUMB;
	}
}

int cmpTypes(int a, int b) {
	if (getType(a) == getType(b)) { return 0; }
	return 1;
}

int cmpTypesList(int src, int size, ...) {
	va_list argp;
	va_start(argp, size);
	while (size-- > 0 ) {
		if (!cmpTypes(src, va_arg(argp, int))) { va_end(argp); return 0; }
	}
	va_end(argp);
	return 1;
}

void mkcall(char *func, long eargs) {
	long fargs; 
	int type = IDfind(func, &fargs); /* yyerror if ID does not exist */
	
	if (getFunc(type) != TFUNC) { yyerror(ERR_NOTFUNC); }
	if (fargs != eargs) { yyerror(ERR_WRONGNARGS); }
}
