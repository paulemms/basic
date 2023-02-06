# An implementation of Dartmouth BASIC (1964)


#' Basic interpreter
#'
#' @param basic_file Name of basic file (default NULL gives interpreter command prompt)
#'
#' @param debug rly debugger
#' @param home_dir home directory for basic files (default is current working directory)
#'
#' @export
#' @examples
#' b <- basic('hello.bas')
#' b$run()
basic <- function(basic_file = NULL, home_dir = getwd(),
                  debug = rly::NullLogger$new()) {

  lexer <- rly::lex(BasicLexer)
  bparser <- rly::yacc(BasicParser)

  # If a filename has been specified, we try to run it
  if (!is.null(basic_file)) {
    basic_file <- file.path(home_dir, basic_file)
    if (!(file.exists(basic_file))) stop('Cannot read file ', basic_file)
    data <- paste(readLines(basic_file), collapse = '\n')
    data <- paste0(data, '\n')
    withCallingHandlers(
      message = function(m) print(m),
      prog <- bparser$parse(data, lexer, debug = debug)
    )
    b <- BasicInterpreter$new(prog)
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
      prog <- bparser$parse(line, lexer, debug = debug)
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
        "NEW" = b <- BasicInterpreter$new(list())
      )
    }
  }

}


#' @export
lex2str <- function(basic_file, home_dir = system.file('scripts', package = 'basic')) {

  basic_file <- file.path(home_dir, basic_file)
  if (!(file.exists(basic_file))) stop('Cannot read file ', basic_file)
  data <- paste(readLines(basic_file), collapse = '\n')

  lexer <- rly::lex(BasicLexer)

  token_list <- list()
  lexer$input(data)
  while(!is.null(token <- lexer$token())) {
    token_list[[length(token_list) + 1]] <- token
  }

  paste(sapply(token_list, function(x) toString(x$value)), collapse = "|")
}
