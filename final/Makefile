LANG=factorial
EXT=fac

LEXDIR=../solex/$(LANG).l
LEX=flex -l
YACC=byacc -dv
BURG=pburg # -T

$(LANG): $(LANG).y $(LEXDIR)
	$(YACC) -dv $(LANG).y
	$(LEX) $(LEXDIR)
	$(BURG) $(LANG).brg
	make -C ../lib
	gcc -o $(LANG) -DYYDEBUG -I../lib lex.yy.c y.tab.c yyselect.c ../lib/libfactorial.a -lm

install:: $(LANG)
	echo "not a delivrable"

clean::
	rm -f *.o $(LANG) lex.yy.c y.tab.c y.tab.h y.output *~ yyselect.c
