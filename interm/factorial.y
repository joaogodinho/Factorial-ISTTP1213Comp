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
#include "tabid.h"

#define TRUE 1
#define FALSE 0
#define TINT 0
#define TSTR 1
#define TNUMB 2
#define TVOID 3

#define concSize6Bit(X) (X & 63)
#define getType(X) (X & 3)
#define getPtr(X) (X & 4)

#define setInt(X) (X | 0)
#define setStr(X) (X | 1)
#define setNumb(X) (X | 2)
#define setVoid(X) (X | 3)
#define setPtr(X) (X | 4)
#define setPub(X) (X | 8)
#define setConst(X) (X | 16)
#define setFunc(X) (X | 32)

#define remPtr(X) (X ^ 4)
#define remPub(X) (X ^ 8)
#define remConst(X) (X ^ 16)
#define remFunc(X) (X ^ 32)

int buildIdent(int keywords, int type, int isPtr, char *ident);
int convertTypes(int type);
int cmpTypes(int a, int b);
char aux_ident[64];
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

%type <i> type_specifier keywords_specifiers init_declarator initializer func_parameters
%type <s> declarator declaration_specifiers 

%%
file			: entry_point						{ IDprint(0, -1); }
			| /* empty file */
			;

entry_point		: declaration
			| entry_point declaration
			;

declaration		: declaration_specifiers ';'				{ if (getType(IDfind($1/*ident*/, (long*)IDtest)) == TVOID) { yyerror("Cannot define void variables");  } }
			| declaration_specifiers init_declarator ';'
			;

declaration_specifiers	: keywords_specifiers type_specifier declarator		{ $$ = $3; } 
			;

keywords_specifiers	: PUBLIC						{ $$ = PUBLIC; }
			| PUBLIC CONST						{ $$ = PUBLIC + CONST; }
			| CONST							{ $$ = CONST; }
			| /* no specifiers */					{ $$ = NONE; }
			;

type_specifier		: VINT							{ $$ = VINT; }
			| VSTR							{ $$ = VSTR; }
			| VNUMB							{ $$ = VNUMB; }
			| VOID							{ $$ = VOID; }
			;
	
init_declarator		: ASG initializer					{ int type = IDfind($<s>0/*ident*/, (long*)IDtest);
										  if (cmpTypes(type, convertTypes($2)/*type*/)) { yyerror("Mismatched types"); } 
										  if (getType(type) == TVOID) { yyerror("Cannot initialize void variables");  } }
			| ASG IDENT						{ if (cmpTypes(IDfind($<s>0/*ident*/, (long*)IDtest), IDfind($2, (long*)IDtest)/*type*/)) { yyerror("Mismatched types"); } }
			| '(' { IDpush(); } func_parameters { IDreplace(IDfind($<s>0/*ident*/, (long*)IDtest), $<s>0/*ident*/, (long)$3/*#param*/); }
							 ')' body		{ IDpop(); }
			;

declarator		: '*' IDENT						{ $$ = $2/*ident*/; buildIdent($<i>-1/*keywords*/, $<i>0/*type*/, TRUE /*pointer*/, $2/*ident*/); }
			| IDENT							{ $$ = $1/*ident*/; buildIdent($<i>-1/*keywords*/, $<i>0/*type*/, FALSE/*pointer*/, $1/*ident*/); }
			;

initializer		: INTEGER						{ $$ = INTEGER; }
			| STRING						{ $$ = STRING; }
			| NUMBER						{ $$ = NUMBER; }
			;

body			: /* no body */
			| '{' body_contents '}'
			;

body_contents		: /* no contents */
			| body_contents parameters
			| body_contents statement
			;

func_parameters		: parameter						{ $$ = 1; }
			| parameter ',' func_parameters				{ $$ = $3 + 1; }
			| /* no parameters */					{ $$ = 0; }
			;

parameters		: parameter ';'
			| parameter ',' parameters
			;

parameter		: type_specifier declarator				/* case where $<i>-1 in line #115 is unknown */
			;

statement		: selection_statement
			| iteration_statement
			| expression ';'
			| statement_body
			| jump_statement
			| left_value '#' expression ';'
			;

statement_body		: '{' { IDpush(); } body_contents '}'			{ IDpop(); }
			;

left_value		: IDENT
			| IDENT '[' expression ']'
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

func_invoc_param	: expression
		 	| expression ',' func_invoc_param
			;

expression		: left_value 
	    		| left_value ASG expression
			| IDENT '(' ')'
			| IDENT '(' func_invoc_param ')'
			| PP left_value 
			| MM left_value
			| left_value PP
			| left_value MM
			| initializer
			| '-' expression %prec UMINUS
			| expression '!'
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
