1.下載後，把它移到課程提供的環境下使用。

2. 執行方式:
	
   - Using .y file to generate *.tab.c and *.tab.h file
	
	bison -d -o y.tab.c LISP.y
	gcc -c -g -I.. y.tab.c 

   - Using .l file to generate *.yy.c file
	
	flex -o lex.yy.c LISP.l
	gcc -c -g -I.. lex.yy.c

   - Link .c generated by yacc and lex
	
	gcc -o LISP y.tab.o lex.yy.o -ll
   
   - Test the data
	
	./LISP < ____.lsp
