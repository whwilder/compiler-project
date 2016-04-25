CC = gcc
CFLAGS = -Wall -O2 -g -std=gnu99

#CFILES = compile.tab.c compile.lex.c syntree.c main.c
#
#HFILES = compile.tab.h syntree.h compile.h
#
#OFILES = compile.lex.o compile.tab.o
#
#compile: $(OFILES) $(HFILES)
#	   $(CC) $(CFLAGS) $(OFILES) -lfl -o compile ${CFILES}
#
#compile.lex.c : compile.l compile.tab.h
#	   flex -o compile.lex.c compile.l
#
#compile.tab.h : compile.y
#	   bison -v -d compile.y
#
#compile.tab.c : compile.y
#	   bison -v -d compile.y

compile : compile.l compile.y main.c
	bison -dvt compile.y
	flex -o compile.lex.c compile.l
	${CC} ${CFLAGS} -o compile compile.tab.c compile.lex.c syntree.c main.c 

.PHONY : clean
clean:
	/bin/rm -f compile compile.lex.c compile.tab.c compile.tab.h a.out compile.output *.o

