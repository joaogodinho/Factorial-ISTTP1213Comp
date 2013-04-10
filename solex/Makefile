LANG=factorial
EXT=fac

LEX=flex -l
YACC=byacc -dv

$(LANG): solex.y $(LANG).l
	$(YACC) -dv solex.y
	$(LEX) -l $(LANG).l
	gcc -o $(LANG) -DYYDEBUG lex.yy.c y.tab.c -lm

install:: $(LANG)
	echo "not a delivrable"

clean::
	rm -f *.o $(LANG) lex.yy.c y.tab.c y.tab.h y.output *~
debug: solex.y $(LANG).l
	$(YACC) -dv solex.y
	$(LEX) -dl $(LANG).l
	gcc -g -o $(LANG) -DYYDEBUG lex.yy.c y.tab.c -lm
