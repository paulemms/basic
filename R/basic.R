# An implementation of Dartmouth BASIC (1964)


#' Get BASIC lexer object
#' @export
basic_lexer <- local({

  cache <- NULL

  function() {
    if (is.null(cache)) cache <<- rly::lex(BasicLexer)
    cache
  }

})

#' Get BASIC parser object
#' @export
basic_parser <- local({

  cache <- NULL

  function() {
    if (is.null(cache)) cache <<- rly::yacc(BasicParser)
    cache
  }

})

#' Basic interpreter
#'
#' @param text Character string containing complete BASIC program (default NULL)
#' @param file Name of basic file (default NULL)
#'
#' @param debug rly debugger
#' @param home_dir home directory for basic files (default is current working directory)
#'
#' @export
#' @examples
#' b <- basic(file='hello.bas', home_dir = system.file('scripts', package = 'basic'))
#' b$run()
basic <- function(text = NULL, file = NULL, home_dir = getwd(), debug = rly::NullLogger$new()) {
  stopifnot(!is.null(text) || !is.null(file))

  lexer <- basic_lexer()
  bparser <- basic_parser()

  # If a filename has been specified, we try to run it
  if (!is.null(file)) {
    basic_file <- file.path(home_dir, file)
    if (!(file.exists(basic_file))) stop('Cannot read file ', basic_file)
    text <- paste(readLines(basic_file), collapse = '\n')
  }
  text <- paste0(text, '\n')

  num_errors <- 0
  withCallingHandlers(
    message = function(m) {
      print(m)
      num_errors <<- num_errors + 1
    },
    prog <- bparser$parse(text, lexer, debug = debug)
  )

  b <- if (num_errors > 0) NULL else BasicInterpreter$new(prog)
  return(invisible(b))
}


#' Interactive Basic interpreter
#'
#' @param debug rly debugger
#'
#' @export
#' @examples
#' basic()
basic_shell <- function(debug = rly::NullLogger$new()) {

  lexer <- basic_lexer()
  bparser <- basic_parser()
  b <- BasicInterpreter$new(list())

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
