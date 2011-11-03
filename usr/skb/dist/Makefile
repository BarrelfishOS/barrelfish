LEX     = flex --header-file=flex.h
YACC    = bison
YFLAGS  = -d -y
objects = parse.o scan.o y.tab.h generator.o 
CFLAGS = -DTEST_PARSER

generator: $(objects)
parse.o: parse.y
scan.o: scan.l
y.tab.h: parse.o scan.o
generator.o: generator.c y.tab.h

clean:
	rm -f $(objects) parse.c scan.c generator y.tab.h


