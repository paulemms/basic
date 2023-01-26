lexer <- rly::lex(BasicLexer)
bparser <- rly::yacc(BasicParser)

test_that("hello script runs", {
  f <- system.file('scripts/hello.bas', package = 'basic')
  data <- paste(readLines(f), collapse = '\n')
  prog <- bparser$parse(data, lexer)
  b <- BasicInterpreter$new(prog)
  x <- capture.output(b$run())
  expect_equal(x, 'HELLO WORLD')
})


test_that("sqrt1 script runs", {
  f <- system.file('scripts/sqrt1.bas', package = 'basic')
  data <- paste(readLines(f), collapse = '\n')
  prog <- bparser$parse(paste0(data, '\n'), lexer)
  b <- BasicInterpreter$new(prog)
  x <- capture.output(b$run())
  expect_length(x, 10)
})

test_that("sqrt2 script runs", {
  f <- system.file('scripts/sqrt2.bas', package = 'basic')
  data <- paste(readLines(f), collapse = '\n')
  prog <- bparser$parse(paste0(data, '\n'), lexer)
  b <- BasicInterpreter$new(prog)
  x <- capture.output(b$run())
  expect_length(x, 10)
})
