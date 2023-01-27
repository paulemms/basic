# An implementation of Dartmouth BASIC (1964)
basic <- function(basic_file = NULL) {

  lexer <- rly::lex(BasicLexer)
  parser <- rly::yacc(BasicParser)

  # If a filename has been specified, we try to run it.
  # If a runtime error occurs, we bail out and enter
  # interactive mode below
  if (!is.null(basic_file)) {
    data <- paste(readLines(basic_file), collapse = '\n')
    data <- paste0(data, '\n')
    prog <- parser$parse(data, lexer, debug = rly::NullLogger$new())
    if (is.null(prog)) stop('Cannot read file ', basic_file)
    b <- BasicInterpreter$new(prog)
    tryCatch(b$run(), error = function(e) print(e))
    return()
  } else {
    b <- BasicInterpreter$new(list())
  }

  # Interactive mode
  while(TRUE) {
    line <- readline("[BASIC] ")
    if (line == "") next
    line <- paste0(line, '\n')
    prog <- parser$parse(line, lexer, debug = rly::NullLogger$new())
    if (is.null(prog)) next
    keys <- names(prog)
    if (is.numeric(keys)) {
      b$add_statements(prog)
    } else {
      stat <- prog[[keys[1]]]
      switch(
        stat[[1]],
        "RUN" = try(b$run()),
        "LIST" = b$list(),
        "BLANK" = b$del_line(stat[[2]]),
        "NEW" = b$erase()
      )
    }
  }

}

