LEX     = flex --header-file=flex.h
YACC    = bison
YFLAGS  = -d -y
CFLAGS = -DTEST_PARSER -std=c99
objects = parse.o scan.o y.tab.h code_generator.o ast.o

code_generator: $(objects)
parse.o: parse.y
scan.o: scan.l
y.tab.h: parse.o scan.o ast.o
code_generator.o: code_generator.c ast.c  y.tab.h

clean:
	rm -f $(objects) parse.c scan.c generator


