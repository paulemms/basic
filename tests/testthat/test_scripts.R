lexer <- rly::lex(BasicLexer)
parser <- rly::yacc(BasicParser)

script <- dir(system.file('scripts', package = 'basic'), full.names = TRUE)

test_that("hello script runs", {
  f <- system.file('scripts/hello.bas', package = 'basic')
  data <- paste(readLines(f), collapse = '\n')
  prog <- parser$parse(data, lexer)
  b <- BasicInterpreter$new(prog)
  x <- capture.output(b$run())
  expect_equal(x, 'HELLO WORLD')
})

test_that("All scripts complete without error", {
  for (f in script) {
    data <- paste(readLines(f), collapse = '\n')
    prog <- parser$parse(paste0(data, '\n'), lexer)
    b <- BasicInterpreter$new(prog)
    expect_no_error(capture.output(b$run()))
  }
})

