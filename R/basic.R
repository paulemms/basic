# An implementation of Dartmouth BASIC (1964)
basic <- function(basic_file = NULL) {

  lexer <- rly::lex(BasicLexer)
  parser <- rly::yacc(BasicParser)
  nolog <- rly::NullLogger$new()

  # If a filename has been specified, we try to run it.
  # If a runtime error occurs, we bail out and enter
  # interactive mode below
  if (!is.null(basic_file)) {
    if (!(file.exists(basic_file))) stop('Cannot read file ', basic_file)
    data <- paste(readLines(basic_file), collapse = '\n')
    data <- paste0(data, '\n')
    withCallingHandlers(
      message = function(m) print(m),
      prog <- parser$parse(data, lexer, debug = nolog)
    )
    b <- BasicInterpreter$new(prog)
    tryCatch(b$run(), error = function(e) print(e))
    return(invisible(b))
  } else {
    b <- BasicInterpreter$new(list())
  }

  # Interactive mode
  while(TRUE) {
    line <- readline("[BASIC] ")
    if (line == "") next
    line <- paste0(line, '\n')
    withCallingHandlers(
      message = function(m) print(m),
      prog <- parser$parse(line, lexer, debug = nolog)
    )
    if (is.null(prog)) next
    keys <- names(prog)
    if (suppressWarnings(!is.na(as.numeric(keys)))) {
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

