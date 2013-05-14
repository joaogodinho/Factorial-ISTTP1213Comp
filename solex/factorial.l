%{ 
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include <errno.h>
#include <limits.h>
#include "node.h"
#include "y.tab.h"
#define TRUE 1
#define FALSE 0
int nComBlock = 0;
int insideStringCom = FALSE;
int needsDelim = FALSE;
void strTreatHex();
void strTreatEscape(char c);
void treatOctal();
void yyerror(char *s);

%} 
%x	COM STR

%% 
"==".*				; /* ignore everything inside comments */
"=<"				nComBlock++; BEGIN COM;
<COM>"=>"			if (!insideStringCom) { if (--nComBlock == 0) { BEGIN INITIAL; } }
<COM>"=<"			if (!insideStringCom) { nComBlock++; }
<COM>\"				insideStringCom = (insideStringCom + 1) % 2;
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

0b[01]+				{ int long numb = strtol(&yytext[2], 0, 2); 
				if (numb < INT_MIN || numb > INT_MAX) { yyerror("integer overflow"); }
				yylval.i = (int) numb;
				needsDelim = TRUE;
				return INTEGER; }

[0-9]+				{ int long numb = strtol(yytext, 0, 10); 
				if (numb < INT_MIN || numb > INT_MAX) { yyerror("integer overflow"); }
				yylval.i = (int) numb; 
				needsDelim = TRUE;
				return INTEGER; }

-{0,1}[0-9]+\.[0-9]+([eE][0-9]+)?	yylval.d = strtod(yytext, NULL); needsDelim = TRUE; printf("%f\n", yylval.d); return NUMBER; 

\"				BEGIN STR;
<STR>\\([0-9a-fA-F]{1,2})	strTreatHex();
<STR>\"				yytext[yyleng-1] = '\0'; yylval.s = strdup(yytext); needsDelim = TRUE; BEGIN INITIAL; return STRING;
<STR>\\n|\\t|\\r|\\\"		strTreatEscape(yytext[yyleng-1]);
<STR>\\.			yyerror("invalid escape character");
<STR>.				yymore();

[-+*\/%<>=|&~]			return *yytext;
[}(\[\],#]			return *yytext;
[)!]				needsDelim = TRUE; return *yytext;
[;{]				needsDelim = FALSE; return *yytext;
\n				if (needsDelim) { yytext[0] = ';'; needsDelim = FALSE; return *yytext; }
[ \t\r]+			; /* whitespace */
.				yyerror("Unknown character");
%% 
int yywrap(void) { if (nComBlock > 0) { yyerror("unfinished comment block"); } else return 1; } 
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

void treatOctal() {
	int i, final = 0, size = yyleng-1;
	int long temp = 0, numb = strtol(&yytext[1], 0, 10);
	for (i = 0; i < size; i++) {
		temp += numb % 10 * pow(8,i);
		numb /= 10;
	}
	if (temp < INT_MIN || temp > INT_MAX) { yyerror("integer overflow"); return; }
	final = temp;
	yylval.i = final;
}
