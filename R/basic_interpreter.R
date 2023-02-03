# This file provides the runtime support for running a basic program

BasicInterpreter <- R6::R6Class(

  classname = "BasicInterpreter",

  public = list(

    prog = NULL,

    functions = NULL,

    vars = NULL,            # All variables
    lists = NULL,            # List variables
    tables = NULL,            # Tables
    loops = NULL,            # Currently active loops
    loopend = NULL,            # Mapping saying where loops end
    gosub = NULL,           # Gosub return point (if any)

    stat = NULL,  # Ordered list of all line numbers
    pc = NULL,                  # Current program counter
    data = NULL,
    dc = NULL,

    # dictionary of names
    names = new.env(hash=TRUE),

    # Initialize the interpreter. prog is a dictionary
    # containing (line,statement) mappings
    initialize = function(prog) {
        self$prog <- prog

        # Built-in function table
        self$functions <- list(
          'SIN' = function(z) sin(self$eval(z)),
          'COS' = function(z) cos(self$eval(z)),
          'TAN' = function(z) tan(self$eval(z)),
          'ATN' = function(z) atan(self$eval(z)),
          'EXP' = function(z) exp(self$eval(z)),
          'ABS' = function(z) abs(self$eval(z)),
          'LOG' = function(z) log(self$eval(z)),
          'SQR' = function(z) sqrt(self$eval(z)),
          'INT' = function(z) as.integer(self$eval(z)),
          'RND' = function(z) runif(1)
        )

    },

    # Collect all data statements
    collect_data = function() {
      self$data <- list()
      for (lineno in self$stat) {
        if (self$prog[[lineno]][[1]] == 'DATA') {
          self$data <- c(self$data, self$prog[[lineno]][[2]])
        }
      }
      self$dc <- 1                  # Initialize the data counter
    },

    # Check for end statements
    check_end = function() {
      has_end <- 0
      for (lineno in self$stat) {
        if (self$prog[[lineno]][[1]] == 'END' && has_end == 0) {
          has_end <- lineno
        }
      }
      if (has_end == 0) {
        e <- simpleError("NO END INSTRUCTION")
        stop(e)
      }
      if (has_end != lineno) {
        e <- simpleError("END IS NOT LAST")
        stop(e)
      }

    },

    # Check loops
    check_loops = function() {
      for (pc in seq_along(self$stat)) {
        lineno <- self$stat[[pc]]
        if (self$prog[[lineno]][[1]] == 'FOR') {
          forinst <- self$prog[[lineno]]
          loopvar <- forinst[[2]]
          flag <- FALSE
          for (i in seq(pc + 1, length(self$stat))) {
            if (self$prog[[self$stat[i]]][[1]] == 'NEXT') {
              nextvar <- self$prog[[self$stat[i]]][[2]]
              if (nextvar != loopvar) next
              assign(as.character(pc), i, envir = self$loopend)
              flag <- TRUE
              break
            }
          }
          if (!flag) {
            e <- errorCondition(sprintf("FOR WITHOUT NEXT AT LINE %s", self$stat[self$pc]),
                                line = self$stat[self$pc])
            stop(e)
          }
        }
      }
    },

    # Evaluate an expression
    eval = function(expr) {
      #browser()
      etype <- expr[[1]]
      if (etype == 'NUM') {
        return(expr[[2]])
      }
      else if (etype == 'GROUP') {
        return (self$eval(expr[[2]]))
      }
      else if (etype == 'UNARY') {
        if (expr[[2]] =='-') return(-self$eval(expr[[3]]))
      }
      else if (etype == 'BINOP') {
        ret <- switch(
          expr[[2]],
          '+' = self$eval(expr[[3]]) + self$eval(expr[[4]]),
          '-' = self$eval(expr[[3]]) - self$eval(expr[[4]]),
          '*' = self$eval(expr[[3]]) * self$eval(expr[[4]]),
          '/' = self$eval(expr[[3]]) / self$eval(expr[[4]]),
          '^' = self$eval(expr[[3]]) ^ self$eval(expr[[4]]),
        )
        return(ret)
      }
      else if (etype == 'VAR') {
        var <- expr[[2]][[1]]
        dim1 <- expr[[2]][[2]]
        dim2 <- expr[[2]][[3]]
        if (is.null(dim1) && is.null(dim2)) {
          if (var %in% names(self$vars)) {
            return(self$vars[[var]])
          } else {
            e <- errorCondition(sprintf("UNDEFINED VARIABLE %s AT LINE %s", var, self$stat[self$pc]),
                                line = self$stat[self$pc])
            stop(e)
          }
        }
        # May be a list lookup or a function evaluation
        if (!is.null(dim1) && is.null(dim2)) {
          if (var %in% names(self$functions)) {
            # A function
            return(self$functions[[var]](dim1))
          } else {
            # A list evaluation
            if (var %in% names(self$lists)) {
              dim1val <- self$eval(dim1)
              if (dim1val < 1 || dim1val > length(self$lists[[var]])) {
                e <- errorCondition(sprintf("LIST INDEX OUT OF BOUNDS AT LINE %s", self$stat[self$pc]),
                                    line = self$stat[self$pc])
                stop(e)
              }
              return(self$lists[[var]][dim1val])
            }
          }
        }
        if (!is.null(dim1) && !is.null(dim2)) {
          if (var %in% names(self$tables)) {
            dim1val <- self$eval(dim1)
            dim2val <- self$eval(dim2)
            if (dim1val < 1 || dim1val > nrow(self$tables[[var]]) || dim2val < 1 ||
                dim2val > ncol(self$tables[[var]])) {
              e <- errorCondition(sprintf("TABLE INDEX OUT OUT BOUNDS AT LINE %s", self$stat[self$pc]),
                                  line = self$stat[self$pc])
              stop(e)
            }
            return(self$tables[[var]][dim1val, dim2val])
          }
        }
        e <- errorCondition(sprintf("UNDEFINED VARIABLE %s AT LINE %s", var, self$stat[self$pc]),
                            line = self$stat[self$pc])
        stop(e)
      }
    },

    # Evaluate a relational expression
    releval = function(expr) {
      etype <- expr[[2]]
      lhs <- self$eval(expr[[3]])
      rhs <- self$eval(expr[[4]])
      flag <- switch(
        etype,
        '<' = lhs < rhs,
        '<=' = lhs <= rhs,
        '>' = lhs > rhs,
        '>=' = lhs >= rhs,
        '=' = lhs == rhs,
        '<>' = lhs != rhs
      )
      return(flag)
    },

    # Assignment
    assign = function(target, value) {
      var <- target[[1]]
      dim1 <- if (length(target) > 1) target[[2]] else NULL
      dim2 <- if (length(target) > 2) target[[3]] else NULL
      if (is.null(dim1) && is.null(dim2)) {
        self$vars[[var]] <- self$eval(value)
      }
      else if (!is.null(dim1) && is.null(dim2)) {
        # List assignment
        dim1val <- self$eval(dim1)
        if (!var %in% names(self$lists)) {
          self$lists[[var]] = rep(0, 10)
        }
        if (dim1val > length(self$lists[[var]])) {
          e <- errorCondition(sprintf("DIMENSION TOO LARGE AT LINE %s", self$stat[self$pc]),
                              line = self$stat[self$pc])
          stop(e)
        }
        self$lists[[var]][dim1val] <- self$eval(value)
      }
      else if (!is.null(dim1) && !is.null(dim2)) {
        dim1val <- self$eval(dim1)
        dim2val <- self$eval(dim2)
        if (!var %in% names(self$tables)) {
          self$tables[[var]] <- matrix(0, 10, 10)
        }
        # Variable already exists
        if (dim1val > nrow(self$tables[[var]]) || dim2val > ncol(self$tables[[var]])) {
          e <- errorCondition(sprintf("DIMENSION TOO LARGE AT LINE %s", self$stat[self$pc]),
                              line = self$stat[self$pc])
          stop(e)
        }
        self$tables[[var]][dim1val, dim2val] <- self$eval(value)

      }
    },

    # Change the current line number
    goto = function(linenum) {
      if (!as.character(linenum) %in% names(self$prog)) {
        e <- errorCondition(sprintf("UNDEFINED LINE NUMBER %d AT LINE %s", linenum, self$stat[self$pc]),
                            line = self$stat[self$pc])
        stop(e)
      }
      self$pc <- match(linenum, self$stat)
    },

    # Run it
    run = function() {
      self$vars <- new.env(hash = TRUE)
      self$lists <- new.env(hash = TRUE)
      self$tables <- new.env(hash = TRUE)
      self$loops <- list()
      self$loopend <- new.env(hash = TRUE)
      self$gosub <- NULL

      self$stat <- objects(self$prog)
      self$stat <- self$stat[order(as.integer(self$stat))]
      self$pc <- 1                  # Current program counter

      # Processing prior to running
      self$collect_data()          # Collect all of the data statements
      self$check_end()
      self$check_loops()

      while (TRUE) {
        line <- self$stat[[self$pc]]
        instr <- self$prog[[line]]

        op <- instr[[1]]
        # END and STOP statements
        if (op %in% c('END', 'STOP')) {
          break           # We're done
        }
        # GOTO statement
        else if (op == 'GOTO') {
          newline <- instr[[2]]
          self$goto(newline)
          next
        }
        # PRINT statement
        else if (op == 'PRINT') {
          plist <- instr[[2]]
          out <- ""
          for (item in plist) {
            label <- item[[1]]
            val <- item[[2]]
            if (out != "") out <- paste0(out, strrep(' ', 15 - nchar(out) %% 15))
            out <- paste0(out, label)
            if (!is.null(val)) {
              if (!is.null(label) && label != "") out <- paste0(out, " ")
              eval <- self$eval(val)
              out <- paste0(out, as.character(eval))
            }
          }
          cat(out)
          end <- instr[[3]]
          if (is.null(end) || !(end == ',' || end == ';')) {
            cat('\n')
          } else if (end == ',') {
            cat(strrep(' ', 15 - nchar(out) %% 15))
          } else if (end == ';') cat(strrep(' ', 3 - nchar(out) %% 3))
        }
        # LET statement
        else if (op == 'LET') {
          self$assign(target = instr[[2]], value = instr[[3]])
        }
        # READ statement
        else if (op == 'READ') {
          for (target in instr[[2]]) {
            if (self$dc <= length(self$data)) {
              value <- list('NUM', self$data[[self$dc]])
              self$assign(target, value)
              self$dc <- self$dc + 1
            } else {
              # No more data.  Program ends
              return()
            }
          }
        }
        else if (op == 'IF') {
          relop <- instr[[2]]
          newline <- instr[[3]]
          if (self$releval((relop))) {
            self$goto(newline)
            next
          }
        }
        else if (op == 'FOR') {
          loopvar <- instr[[2]]
          initval <- instr[[3]]
          finval <- instr[[4]]
          stepval <- instr[[5]]

          # Check to see if this is a new loop
          if (length(self$loops) == 0 || self$loops[[length(self$loops)]][1] != self$pc) {
            # Looks like a new loop. Make the initial assignment
            newvalue <- initval
            self$assign(list(loopvar, NULL, NULL), initval)
            if (is.null(stepval)) stepval <- list('NUM', 1)
            stepval <- self$eval(stepval)    # Evaluate step here
            self$loops[[length(self$loops) + 1]] <- list(self$pc, stepval)
          } else {
            # It's a repeat of the previous loop
            # Update the value of the loop variable according to the
            # step
            self$vars$I
            stepval <- list('NUM', self$loops[[length(self$loops)]][[2]])
            newvalue <- list('BINOP', '+', list('VAR', list(loopvar, NULL, NULL)), stepval)
            relop <- if (self$loops[[length(self$loops)]][[2]] < 0) '>=' else '<='
            if (!self$releval(list('RELOP', relop, newvalue, finval))) {
              # Loop is done. Jump to the NEXT
              self$pc <- as.integer(self$loopend[[as.character(self$pc)]])
              self$loops <- self$loops[-length(self$loops)]
            } else {
              self$assign(list(loopvar, NULL, NULL), newvalue)
            }
          }
        }
        else if (op == 'NEXT') {
          if (length(self$loops) == 0) {
            e <- errorCondition(sprintf("NEXT WITHOUT FOR AT LINE %s", line),
                             line = line)
            stop(e)
          }
          nextvar <- instr[[2]]
          self$pc <- self$loops[[length(self$loops)]][[1]]
          loopinst <- self$prog[[self$stat[self$pc]]]
          forvar <- loopinst[[2]]
          if (nextvar != forvar) {
            e <- errorCondition(sprintf("NEXT DOESN'T MATCH FOR AT LINE %s", line),
                                line = line)
            stop(e)
          }
          next
        }
        else if (op == 'GOSUB') {
          newline <- instr[[2]]
          if (!is.null(self$gosub)) {
            e <- errorCondition(sprintf("ALREADY IN A SUBROUTINE AT LINE %s", line),
                                line = line)
            stop(e)
          }
          self$gosub <- self$stat[self$pc]
          self$goto(newline)
          next
        }
        else if (op == 'RETURN') {
          if (is.null(self$gosub)) {
            e <- errorCondition(sprintf("RETURN WITHOUT A GOSUB AT LINE %s", line),
                             line = line)
            stop(e)
          }
          self$goto(self$gosub)
          self$gosub <- NULL
        }
        else if (op == 'FUNC') {
          fname <- instr[[2]]
          pname <- instr[[3]]
          expr <- instr[[4]]
          eval_func <- function(pvalue) {
            self$assign(list(pname, NULL, NULL), pvalue)
            return(self$eval(expr))
          }
          self$functions[[fname]] <- eval_func
        }
        else if (op == 'DIM') {
          el <- instr[[2]]
          for (j in seq(1, length(el), by = 3)) {
            vname <- el[[j]]
            x <- el[[j+1]]
            y <- el[[j+2]]
            if (y ==0) {
              # Single dimension variable
              self$lists[[vname]] <- rep(0, x)
            } else {
              # Double dimension variable
              self$tables[[vname]] <- matrix(0, x, y)
            }
          }
        }

        self$pc <- self$pc + 1

      }
    },

    # # Utility functions for program listing
    expr_str = function(expr) {
      switch(
        expr[[1]],
        'NUM' = as.character(expr[[2]]),
        'GROUP' = sprintf('(%s)', self$expr_str(expr[[2]])),
        'UNARY' = if (expr[[2]] == '-') return(paste0('-', expr[[2]])),
        'BINOP' = return(sprintf('%s %s %s', self$expr_str(expr[[3]]), expr[[2]],
                                 self$expr_str(expr[[4]]))),
        'VAR' = return(self$var_str(expr[[2]]))
      )
    },

    relexpr_str = function(expr) {
      return(sprintf("%s %s %s", self$expr_str(expr[[3]]), expr[[3]], self$expr_str(expr[[4]])))
    },

    var_str = function(var) {
      varname <- var[[1]]
      dim1 <- var[[2]]
      dim2 <- var[[3]]
      if (is.null(dim1) && is.null(dim2)) return(varname)
      if (!is.null(dim1) && is.null(dim2)) return(sprintf("%s(%s)", varname, self$expr_str(dim1)))
      return(sprintf("%s(%s,%s)", varname, self$expr_str(dim1), self$expr_str(dim2)))
    },

    # Create a program listing
    list = function() {
      stat <- names(self$prog)      # Ordered list of all line numbers
      stat <- as.character(sort(as.integer(stat)))
      for (line in stat) {
        instr <- self$prog[[line]]
        op <- instr[[1]]
        txt <- switch(
          op,
          'END' = ,
          'STOP' = ,
          'RETURN'= sprintf("%s %s", line, op),
          'REM' = sprintf("%s %s", line, instr[[2]]),
          'PRINT' = {
            out <- sprintf("%s %s ", line, op)
            first <- TRUE
            for (p in instr[[2]]) {
              if (!first) out <- paste0(out, ", ")
              if (!is.null(p[[1]]) && !is.null(p[[2]])) {
                out <- paste0(out, sprintf('"%s"%s', p[[1]], self$expr_str(p[[2]])))
              } else if (!is.null(p[[2]])) {
                out <- paste0(out, self$expr_str(p[[2]]))
              } else {
                out <- paste0(out, sprintf('"%s"', p[[1]]))
              }
              first <- FALSE
            }
            out <- paste0(out, instr[[3]])
            sprintf(out)
            },
          'LET' = sprintf("%s LET %s = %s", line, self$var_str(instr[[2]]),
                            self$expr_str(instr[[3]])),
          'READ' = sprintf("%s READ %s", line, paste(lapply(instr[[2]], self$var_str), collapse = ',')),
          'IF' =  sprintf("%s IF %s THEN %d", line, self$relexpr_str(instr[[2]]), instr[[3]]),
          'GOTO' = ,
          'GOSUB' = sprintf("%s %s %s", line, op, instr[[2]]),
          'FOR' = {
            out <- sprintf("%s FOR %s = %s TO %s",
                line, instr[[2]], self$expr_str(instr[[3]]), self$expr_str(instr[[4]]))
            if (!is.null(instr[[5]]))
              out <- paste0(out, sprintf(" STEP %s", self$expr_str(instr[[5]])))
            sprintf(out)
          },
          'NEXT' = sprintf("%s NEXT %s", line, instr[[2]]),
          'FUNC' =  sprintf("%s DEF %s(%s) = %s", line, instr[[2]], instr[[3]], self$expr_str(instr[[4]])),
          'DIM' = {
            out <- lapply(split(instr[[2]], ceiling(seq_along(instr[[2]])/3)),
                   function(x) if (x[[3]] == 0) sprintf("%s(%d)", x[[1]], x[[2]]) else
                     sprintf("%s(%d,%d)", x[[1]], x[[2]], x[[3]]))
            sprintf("%s DIM %s", line, paste(out, collapse = ','))
          },
          'DATA' = {
            out <- sprintf("%s DATA ", line)
            first <- TRUE
            for (v in instr[[2]]) {
              if (!first) out <- paste0(out, ",")
              first <- 0
              out <- paste0(out, v)
            }
            sprintf(out)
          },
          stop()
        )
        cat(txt, '\n')
      }
    },

    # Erase the current program
    erase = function() {self$prog <- NULL},
    #
    # Insert statements
    add_statements = function(prog) {
      for (line in names(prog)) {
        self$prog[[line]] <- prog[[line]]
      }
    },

    # Delete a statement
    del_line = function(lineno) {
      try(self$prog[[lineno]] <- NULL)
    }
  )
)

