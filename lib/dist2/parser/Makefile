LEX     = flex --header-file=flex.h
YACC    = bison -d
YFLAGS  = -d -y
CFLAGS = -DTEST_PARSER -std=c99 
objects = parse.o scan.o y.tab.h test_parser.o ast.o

test_parser: $(objects)
parse.o: parse.y
scan.o: scan.l
flex.h: scan.o
y.tab.h: parse.o scan.o ast.o
test_parser.o: test_parser.c ast.c y.tab.h flex.h

clean:
	rm -f $(objects) parse.c scan.c test_parser


