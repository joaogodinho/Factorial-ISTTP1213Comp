LANG=factorial
EXT=fac

LEXDIR=../solex/$(LANG).l
LEX=flex -l
YACC=byacc -dv

$(LANG): $(LANG).y $(LEXDIR)
	$(YACC) -dv $(LANG).y
	$(LEX) -l $(LEXDIR)
	gcc -o $(LANG) -DYYDEBUG lex.yy.c y.tab.c -lm

install:: $(LANG)
	echo "not a delivrable"

clean::
	rm -f *.o $(LANG) lex.yy.c y.tab.c y.tab.h y.output *~
