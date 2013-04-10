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
#define YYDEBUG 1
%}

%union {
	int i;			/* integer value */
	char *s;		/* symbol name or string literal */
	double d;		/* number value */
};

%token <i> INTEGER
%token <s> VARIABLE STRING
%token <d> NUMBER
%token VOID VINT VSTR PUBLIC VNUMB CONST IF THEN ELSE WHILE
%token DO FOR IN STEP UPTO DOWNTO BREAK CONTINUE IDENT STRING

%nonassoc IFX '(' ')' '[' ']' PP MM '~' '!'
%nonassoc ELSE

%right ASG
%left '|' '&' 
%left GE LE NE '>' '<' '='
%left '+' '-'
%left '*' '/' '%' 
%nonassoc UMINUS

%%
file			: entry_point
			|
			;

entry_point		: declaration
			| entry_point declaration
			;

declaration		: declaration_specifiers ';'
			| declaration_specifiers init_declarator ';'
			;

declaration_specifiers	: keywords_specifiers type_specifier declarator
			;

keywords_specifiers	: PUBLIC
			| PUBLIC CONST
			| CONST
			|
			;

type_specifier		: VINT
			| VSTR
			| VNUMB
			| VOID
			;
	
init_declarator		: ASG initializer
			| ASG IDENT
			| '(' func_parameters ')' body
			| '(' func_parameters ')'
			;

declarator		: '*' IDENT
			| IDENT
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
int main() {
	yydebug = 1;
	yyparse();
  	return 0;
}

void yyerror(char *s) {
	printf("%s\n", s);
}

char **yynames =
#if YYDEBUG > 0
         (char**)yyname;
#else
         0;
#endif