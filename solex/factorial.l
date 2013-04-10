%{ 
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include <errno.h>
#include "y.tab.h"
#define TRUE 1
#define FALSE 0
int nComBlock = 0;
int needsDelim = FALSE;
void strTreatHex();
void strTreatEscape(char c);
int treatOctal();
void yyerror(char *s);

%} 
%x	COM STR

%% 
"==".*				; /* ignore everything inside comments */
"=<"				nComBlock++; BEGIN COM;
<COM>"=>"			if (--nComBlock == 0) { BEGIN INITIAL; }
<COM>"=<"			nComBlock++;
<COM>.|\n			; /* ignore everything inside comments */

">="				return GE;
"<="				return LE;
"<>"				return NE;
":="				return ASG;
"++"				return PP;
"--"				return MM;

"void"				return VOID;
"integer"			return VINT;
"string"			return VSTR;
"public"			return PUBLIC;
"number"			return VNUMB;
"const"				return CONST;
"if"				return IF;
"then"				needsDelim = FALSE; return THEN;
"else"				needsDelim = FALSE; return ELSE;
"while"				return WHILE;
"do"				return DO;
"for"				return FOR;
"in"				return IN;
"step"				return STEP;
"upto"				return UPTO;
"downto"			return DOWNTO;
"break"				return BREAK;
"continue"			return CONTINUE;

[A-Za-z][A-Za-z0-9_]*		yylval.s = strdup(yytext); needsDelim = TRUE; return IDENT;

0[0-9]+				treatOctal(); needsDelim = TRUE; return INTEGER;

0b[01]+				{ yylval.i = strtol(&yytext[2], 0, 2);
				needsDelim = TRUE;
				if (errno == ERANGE) { yyerror("integer overflow"); }
				else { return INTEGER; } }

[0-9]+				{ yylval.i = strtol(yytext, 0, 10); 
				needsDelim = TRUE;
				if (errno == ERANGE) { yyerror("integer overflow"); }
				else { return INTEGER; } }

[0-9]+\.[0-9]+([eE][0-9]+)?	yylval.d = strtod(yytext, NULL); needsDelim = TRUE; return NUMBER; 

\"				BEGIN STR;
<STR>\\([0-9a-fA-F]{1,2})	strTreatHex();
<STR>\"				yytext[yyleng-1] = '\0'; needsDelim = TRUE; BEGIN INITIAL; return STRING;
<STR>\\n|\\t|\\r|\\\"		strTreatEscape(yytext[yyleng-1]);
<STR>.				yymore();

[-+*\/%<>=|&~]			return *yytext;
[}(\[\],#]			return *yytext;
[)!]				needsDelim = TRUE; return *yytext;
[;{]				needsDelim = FALSE; return *yytext;
\n				if (needsDelim) { yytext[0] = ';'; needsDelim = FALSE; return *yytext; }
[ \t\r]+			; /* whitespace */
.				yyerror("Unknown character");
%% 
int yywrap(void) { return 1; } 
char *getyytext() { return yytext; }

void strTreatHex() {
	int i = yyleng;	
	while (yytext[i] != '\\') { i--; }
	yytext[i] = strtol(&yytext[i+1], 0, 16);
	yytext[i+1] = '\0';
	yymore();
}

void strTreatEscape(char c) {
	switch (c) {
		case 'n': yytext[yyleng-2] = '\n'; break;
		case 't': yytext[yyleng-2] = '\t'; break;
		case 'r': yytext[yyleng-2] = '\r'; break;
		case '"': yytext[yyleng-2] = '\"'; break;
	}
	yytext[yyleng-1] = '\0';
	yymore();
}

int treatOctal() {
	int i, final = 0, size = yyleng-1;
	long int numb = strtol(&yytext[1], 0, 10);
	for (i = 0; i < size; i++) {
		final += numb % 10 * pow(8,i);
		numb /= 10;
	}
	printf("%d\n", final);
	yylval.i = final;
	if (errno == ERANGE) { yyerror("integer overflow"); }
	else { return INTEGER; }
}
