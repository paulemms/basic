# TODO

rm(list = ls())
debuglog <- rly::RlyLogger$new()
nolog <- rly::NullLogger$new()
devtools::load_all()

data <- '10 LET I = 2\n 20 LET I = I + 1\n 30 PRINT I\n 999 END \n'
data <- '10 IF 20<=30 THEN 50\n 20 PRINT 22\n 50 END\n'
data <- '5 READ A\n 10 PRINT A\n 20 DATA -7\n 999 END \n'
data <- paste0(paste(readLines('inst/scripts/linear.bas'), collapse = '\n'), '\n')
data <- '5 LET PRINT 2*(1.5+SIN((2)))\n 999 END \n'
data <- '5 DIM A(A,2)\n 10 LET X=DIM \n 20 FOR I = 1 TO 10\n999 END \n'
data <- '10 FOR I = 1 TO 20\n 15 FOR J = 1 TO 20\n 20 PRINT I*J;\n 30 NEXT J\n 40 NEXT I\n999 END \n'
data <- '10 LET X = 0\n 20 LET X = X + 1\n 30 PRINT SIN(X), SQR(X)\n40 IF X < 100 THEN 20\n 999 END\n'
lexer <- basic_lexer()
parser <- basic_parser()
withCallingHandlers(
  error = function(e) print(e),
  message = function(m) print(m),
  prog <- parser$parse(data, lexer, debug = nolog)
)

# don't run if any parser errors
if (!any(sapply(prog, is.null))) {
  b <- BasicToC$new(prog)
  print(b$convert())
  b$list()
}


