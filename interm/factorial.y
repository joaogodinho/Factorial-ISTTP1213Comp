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

#define concSize6Bit(X) (X & 63)
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

%type <i> declaration_specifiers type_specifier declarator keywords_specifiers

%%
file			: entry_point
			| /* empty file */
			;

entry_point		: declaration
			| entry_point declaration
			;

declaration		: declaration_specifiers ';'
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
	
init_declarator		: ASG initializer					/* must not be void */
			| ASG IDENT						/* types must match */
			| '(' func_parameters ')' body	
			| '(' func_parameters ')'
			;
										/* force size to 6bit, $-1 may be random/unknown value, due to parameter rule */
declarator		: '*' IDENT						{ $$ = buildIdent(concSize6Bit($<i>-1)/*keywords*/, $<i>0/*type*/, TRUE /*pointer*/, $2 /*ident*/); }
			| IDENT							{ $$ = buildIdent(concSize6Bit($<i>-1)/*keywords*/, $<i>0/*type*/, FALSE/*pointer*/, $1 /*ident*/); }
			;

initializer		: INTEGER
			| STRING
			| NUMBER
			;

body			: '{' body_contents '}'	
			;

body_contents		: 
			| body_contents parameters
			| body_contents statement
			;

func_parameters		: parameter
			| parameter ',' func_parameters
			|
			;

parameters		: parameter ';'
			| parameter ',' parameters
			;

parameter		: type_specifier declarator
			;

statement		: selection_statement
			| iteration_statement
			| expression ';'
			| body
			| jump_statement
			| left_value '#' expression ';'
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
	//printf("keywords: %i\ntype: %i\nident %s\n", keywords, type, ident);

	switch (keywords) {
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
	//printf("var: %i\n", var);
	return var;
}
