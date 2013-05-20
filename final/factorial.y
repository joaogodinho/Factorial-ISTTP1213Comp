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

int pos; /* local variable offset (no functions inside a function) */
int lbl; /* label counter for generated labels */
extern void declare(NODEPTR_TYPE p, NODEPTR_TYPE p2);
extern void pushParam(NODEPTR_TYPE expr);
char *mklbl(int n);
int makeParam(int isArg, int nArgs, int offset, NODEPTR_TYPE type, NODEPTR_TYPE ident);
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
%token PUBLIC_CONST EMPTY DECL_SPEC IDENT_PTR FUNC PARAM JZ JMP ETIQ LABEL END CALL ARGINTEGER ARGSTRING ARGNUMBER INDEX
%token LOCAL JNZ NEG

%nonassoc IFX '(' ')' '[' ']' PP MM '~' '!'
%nonassoc ELSE

%right ASG
%left '|' '&' 
%left GE LE NE '>' '<' '='
%left '+' '-'
%left '*' '/' '%' 
%nonassoc UMINUS

%type <n> decl_spec decl_init key_spec typ_spec ident type param body body_cont stmt select_stmt expr iter_stmt jump_stmt stmt_body left_val func_invoc_param
%type <i> decl_func_param params for_cond for_step
%%
file			: decls
			;

decls			: 
			| decls decl_spec ';'					{ declare($2, 0); }
			| decls decl_spec decl_init ';'				{ declare($2, $3); }
			| decls decl_spec '(' 
					{ if (OP_LABEL(LEFT_CHILD(RIGHT_CHILD($2))) == VOID) { IDnew(FUNC, RIGHT_CHILD(RIGHT_CHILD($2))->value.s, 0); }
					  else { IDnew(FUNC, RIGHT_CHILD(RIGHT_CHILD($2))->value.s, 1); } pos = 0; IDpush(); } 
			  decl_func_param ')' body ';'				{ function(RIGHT_CHILD(RIGHT_CHILD($2))->value.s, -pos, $7); pos = 0; IDpop(); }
			;

decl_spec		: key_spec typ_spec ident				{ $$ = binNode(DECL_SPEC, $1, binNode(DECL_SPEC, $2, $3)); }
			;

key_spec		: PUBLIC						{ $$ = nilNode(PUBLIC); }
			| PUBLIC CONST						{ $$ = nilNode(PUBLIC_CONST); }
			| CONST							{ $$ = nilNode(CONST); }
			| /* no specifiers */					{ $$ = nilNode(EMPTY); }
			;

typ_spec		: VINT							{ $$ = nilNode(VINT); }
			| VSTR							{ $$ = nilNode(VSTR); }
			| VNUMB 						{ $$ = nilNode(VNUMB); }
			| VOID 							{ $$ = nilNode(VOID); }
			;

ident			: '*' IDENT						{ $$ = strNode(IDENT_PTR, $2); }
			| IDENT							{ $$ = strNode(IDENT, $1); }
			;

decl_init		: ASG type						{ $$ = $2; }
			| ASG IDENT						{ $$ = strNode(IDENT, $2); }
			;

type			: INTEGER						{ $$ = intNode(INTEGER, $1); }
			| STRING						{ $$ = strNode(STRING, $1); }
			| NUMBER						{ $$ = realNode(NUMBER, $1); }
			;

decl_func_param		: /* no params */					{ $$ = 0; }
			| decl_func_param ',' param				{ $$ = makeParam(1, $1, 8, LEFT_CHILD($3), RIGHT_CHILD($3)) ; }
			| param							{ $$ = makeParam(1, 0, 8, LEFT_CHILD($1), RIGHT_CHILD($1)) ; }
			;

body			: /* no body */						{ $$ = 0; }
			| '{' body_cont '}'					{ $$ = $2; }
			;

body_cont		: /* no contents */					{ $$ = nilNode(END); }
			| body_cont stmt					{ $$ = binNode(';', $1, $2); }
			;

param			: typ_spec ident					{ $$ = binNode(PARAM, $1, $2); }
			;

params			: params ',' param					{ $$ = makeParam(0, $1, 4, LEFT_CHILD($3), RIGHT_CHILD($3)); }
			| param							{ $$ = makeParam(0, 0, 4, LEFT_CHILD($1), RIGHT_CHILD($1)); }
			;

stmt			: select_stmt						{ $$ = $1; }
			| iter_stmt						{ $$ = $1; }
			| jump_stmt						{ $$ = $1; }
			| stmt_body						{ $$ = $1; }
			| left_val '#' expr ';'					{ $$ = $1; }
			| expr ';'						{ $$ = $1; }
			| params ';'						{ $$ = nilNode(PARAM); }
			;

select_stmt		: IF expr THEN stmt %prec IFX				{ int lbl1 = ++lbl; 
										$$ = seqNode(';', 3, 
											binNode(JZ, $2/* expr  */, 
											strNode(ETIQ, mklbl(lbl1))), $4/* stmt */, 
											strNode(LABEL, mklbl(lbl1))); }
			| IF expr THEN stmt ELSE stmt				{ int lbl1 = ++lbl, lbl2 = ++lbl;
										$$ = seqNode(';', 6,
											binNode(JZ, $2/* expr */, strNode(ETIQ, mklbl(lbl1))), $4/* stmt */,
											strNode(JMP, mklbl(lbl2)), 
											strNode(LABEL, mklbl(lbl1)), $6 /* else stmt */,
											strNode(LABEL, mklbl(lbl2))); }
			;

iter_stmt		: DO stmt WHILE expr ';'				{ int lbl1 = ++lbl, lbl2 = ++lbl;
										$$ = seqNode(';', 4,								
											strNode(LABEL, mklbl(lbl2)),
											$2 /* stmt */,
											strNode(LABEL, mklbl(lbl1)),
											binNode(JNZ, $4, strNode(ETIQ, mklbl(lbl2)))); }
			| FOR left_val IN expr for_cond expr for_step DO stmt	{ $$ = nilNode(FOR); }
			;

for_cond		: UPTO							{ $$ = 0; }
			| DOWNTO						{ $$ = 0; }
			;
for_step		: STEP expr						{ $$ = 0; }
			| /* normal step */					{ $$ = 0; }
			;

jump_stmt		: BREAK ';'						{ $$ = 0; }
			| BREAK INTEGER ';'					{ $$ = 0; }
			| CONTINUE ';'						{ $$ = 0; }
			| CONTINUE INTEGER ';'					{ $$ = 0; }
			;

stmt_body		: '{' body_cont '}'					{ $$ = $2; }
			;

left_val		: IDENT							{ $$ = mkvar($1); }
			| IDENT '[' expr ']'					{ $$ = binNode(INDEX, mkvar($1), $3); }
			;

expr			: left_val						{ $$ = $1; }
			| PP left_val						{ $$ = $2; }
			| MM left_val						{ $$ = $2; }
			| left_val PP						{ $$ = $1; }
			| left_val MM						{ $$ = $1; }
			| left_val ASG expr					{ $$ = binNode(ASG, $1, $3); }
			| IDENT '(' func_invoc_param ')'			{ $$ = binNode(CALL, strNode(IDENT, $1), $3); }
			| type							{ $$ = $1; }
			| '-' expr %prec UMINUS					{ $$ = uniNode(NEG, $2); }
			| expr '!'						{ $$ = $1; }
			| '~' expr						{ $$ = $2; }
			| '*' expr						{ $$ = $2; }
			| '&' expr						{ $$ = $2; }
			| expr '+' expr						{ $$ = binNode('+', $1, $3); }
			| expr '-' expr						{ $$ = binNode('-', $1, $3); }
			| expr '*' expr						{ $$ = binNode('*', $1, $3); }
			| expr '/' expr						{ $$ = binNode('/', $1, $3); }
			| expr '%' expr						{ $$ = binNode('%', $1, $3); }
			| expr '<' expr						{ $$ = binNode('<', $1, $3); }
			| expr '>' expr						{ $$ = binNode('>', $1, $3); }
			| expr '|' expr						{ $$ = binNode('|', $1, $3); }
			| expr '&' expr						{ $$ = binNode('&', $1, $3); }
			| expr '=' expr						{ $$ = binNode('=', $1, $3); }
			| expr GE expr						{ $$ = binNode(GE, $1, $3); }
			| expr LE expr						{ $$ = binNode(LE, $1, $3); }
			| expr NE expr						{ $$ = binNode(NE, $1, $3); }
			| '(' expr ')'						{ $$ = $2; }
			;

func_invoc_param	: /* no args */						{ $$ = nilNode(END); }
			| func_invoc_param ',' expr				{ $$ = binNode(',', $1, $3); }
			| expr							{ $$ = $1; }
			;
%%

int makeParam(int isArg, int nArgs, int offset, NODEPTR_TYPE type, NODEPTR_TYPE ident) {
	switch (OP_LABEL(type)) {
		case VINT: 
			if (isArg) {
				IDnew(ARGINTEGER, ident->value.s, offset + nArgs * 4);
			} else {
				IDnew(INTEGER, ident->value.s, pos -= 4); 
			}
			return ++nArgs;
		case VSTR: 
			if (isArg) {
				IDnew(ARGSTRING, ident->value.s,  offset + nArgs * 4);
			} else {
				IDnew(STRING, ident->value.s, pos -= 4);
			}
			return ++nArgs;
		case VNUMB: 
			if (isArg) {
				IDnew(ARGNUMBER, ident->value.s, offset + nArgs * 4); 
			} else {
				IDnew(NUMBER, ident->value.s, pos -= 8); 
			}
			return nArgs + 2;
	}
}

char *mklbl(int n) {
  static char buf[20];
  sprintf(buf, "_i%d", n);
  return strdup(buf);
}

static Node *mkvar(char *name) {
	long loc;
	int type = IDfind(name, &loc);
	if (type == FUNC) { return intNode(LOCAL, 0); }
	else if ( loc != 0) { return intNode(LOCAL, loc); }
	else { return strNode(IDENT, name); }
}

char **yynames =
#if YYDEBUG > 0
	(char**) yyname;
#else
	0;
#endif
