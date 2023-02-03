# This file provides the runtime support for running a basic program

# TODO
# need newline at END - python doesnt seem to need this
# use snapshots for test output?

library(rly)
rm(list = ls())
debuglog <- rly::RlyLogger$new()
nolog <- rly::NullLogger$new()
devtools::load_all()

#prog <- basic_parse(, debug = nolog)
#prog <- basic_parse('5 PRINT "hi"\n 999 END \n', debug = debuglog)
#prog <- basic_parse('5 LET A = 1\n 10 PRINT SIN(A)\n 999 END \n', debug = debuglog)
#prog <- basic_parse('5 DIM A(2,2)\n 10 PRINT A(1,1)\n 999 END \n', debug = debuglog)
#prog <- basic_parse('5 DIM A(2)\n 10 GOTO 998\n 999 END \n')#, debug = debuglog)
#prog <- basic_parse('5 READ A,B,C\n 10 DATA 9,10,11\n 20 PRINT A,B,C\n 999 END \n')#, debug = debuglog)
#prog <- basic_parse('5 DEF F(X) = X^X\n 10 PRINT F(3)^2\n 999 END \n', debug = debuglog)
#prog <- basic_parse('5 DIM A(2)\n 10 PRINT A(1)\n 999 END \n', debug = debuglog)
#prog <- basic_parse("10 GOSUB 30\n 20 GOTO 50 \n30 PRINT 10\n 40 RETURN\n 50 END\n")

data <- '10 LET I = 2\n 20 LET I = I + 1\n 30 PRINT I\n 999 END \n'
data <- '10 LET X = 0\n 20 LET X = X + 1\n 30 PRINT X, SQR(X)\n40 IF X < 100 THEN 20\n 999 END\n'
data <- '10 IF 20<=30 THEN 50\n 20 PRINT 22\n 50 END\n'
data <- '5 READ A\n 10 PRINT A\n 20 DATA -7\n 999 END \n'
data <- '5 LET PRINT 2*(1.5+SIN((2)))\n 999 END \n'
data <- '5 DIM A(A,2)\n 10 LET X=DIM \n 20 FOR I = 1 TO 10\n999 END \n'
data <- '10 FOR I = 1 TO 20\n 15 FOR J = 1 TO 20\n 20 PRINT I*J;\n 30 NEXT J\n 40 NEXT I\n999 END \n'
data <- '5 DIM A(50,15), B(20) \n 20 PRINT B(1)\n999 END \n'
data <- paste0(paste(readLines('inst/scripts/linear.bas'), collapse = '\n'), '\n')
lexer <- rly::lex(BasicLexer)
parser <- rly::yacc(BasicParser)
withCallingHandlers(
  error = function(e) print(e),
  message = function(m) print(m),
  prog <- parser$parse(data, lexer, debug = nolog)
)
# don't run if any parser errors
if (!any(sapply(prog, is.null))) {
  b <- BasicInterpreter$new(prog)
  b$run()
  b$list()
}


# lexer <- rly::lex(BasicLexer)
# lexer$input("5 PxRINT 2\n 999 END \n")
# print(lexer$token())
# print(lexer$token())
# print(lexer$token())
# print(lexer$token())

