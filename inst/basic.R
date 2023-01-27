# An implementation of Dartmouth BASIC (1964)
#
rm(list = ls())
library(rly)
devtools::load_all()

# If a filename has been specified, we try to run it.
# If a runtime error occurs, we bail out and enter
# interactive mode below
args <- commandArgs(TRUE)
print(args)
if (length(args) == 1) {
  browser()
  data <- paste(readLines(args[1]), collapse = '\n')
  prog <- basic_parse(data)
  if (!prog) dtop('Cannot read file ', args[1])
  b <- BasicInterpreter$new(prog)
  tryCatch(b$run, error = function(e) e)
} else {
    b <- BasicInterpreter$new(list())
}

# Interactive mode
while(TRUE) {
    line <- readline("[BASIC] ")
    if (line == "") next
    line <- paste0(line, '\n')
    prog <- basic_parse(line)
    if (is.null(prog)) next
    keys <- names(prog)
    browser()
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
