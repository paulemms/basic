# TODO
# need newline at END - python doesnt seem to need this
# use snapshots for test output?

library(rly)
rm(list = ls())
debuglog <- rly::RlyLogger$new()
nolog <- rly::NullLogger$new()
devtools::load_all()


data <- paste0(paste(readLines('inst/scripts/linear.bas'), collapse = '\n'), '\n')
data <- "10 GOSUB 30\n 20 GOTO 50 \n30 PRINT 32\n 40 RETURN\n 50 END\n"
b <- basic(data, debug = nolog)

# don't run if any parser errors
if (!is.null(b)) {
  b$run()
  b$list()
}


