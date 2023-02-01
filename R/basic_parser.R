# An implementation of Dartmouth BASIC (1964)
#

BasicParser <- R6::R6Class(

  classname = "BasicParser",

  public = list(

    tokens = TOKENS,

    # Parsing rules
    precedence = list(
      c('left', 'PLUS', 'MINUS'),
      c('left', 'TIMES', 'DIVIDE'),
      c('left', 'POWER'),
      c('right', 'UMINUS')
    ),

    errors = list(),

    # A BASIC program is a series of statements.  We represent the program as a
    # dictionary of tuples indexed by line number.

    p_program = function(doc='program : program statement
                                      | statement', p) {
      if (p$length() == 2 && !is.null(p2 <- p$get(2))) {
        e <- new.env(hash = TRUE)
        p$set(1, e)
        assign(as.character(p2[[1]]), p2[[2]], envir = e)
      } else if (p$length() == 3) {
        p$set(1, p$get(2))
        if (is.null(p$get(1))) {
          p$set(1, new.env())
        }
        if (!is.null(p3 <- p$get(3))) {
          assign(as.character(p3[[1]]), p3[[2]], envir = p$get(1))
        }
      }
    },

    # This catch-all rule is used for any catastrophic errors.  In this case,
    # we simply return nothing
    p_program_error = function(doc='program : error', p) {
      e <- errorCondition('Fatal error')
      signalCondition(e)
      p$set(1, NULL)
    },

    # Format of all BASIC statements.
    p_statement = function(doc='statement : INTEGER command NEWLINE', p) {
      if (is.character(p$get(3))) {
        m <- messageCondition(sprintf("%s %s %s", p$get(3), "AT LINE", p$get(2)),
                              line = as.integer(p$get(2)))
        signalCondition(m)
        p$set(1, NULL)
      } else {
        lineno = as.integer(p$get(2))
        p$set(1, list(lineno, p$get(3)))
      }
    },

    # Interactive statements.
    p_statement_interactive = function(doc='statement : RUN NEWLINE
                                                      | LIST NEWLINE
                                                      | NEW NEWLINE', p) {
      p$set(1, list(p$get(2), p$get(2)))
    },

    # Blank line number
    p_statement_blank = function(doc='statement : INTEGER NEWLINE', p) {
      p$set(1, list(0, list('BLANK', as.integer(p$get(2)))))
    },


    # Error handling for malformed statements
    p_statement_bad = function(doc='statement : INTEGER error NEWLINE', p) {
      m <- messageCondition(sprintf("MALFORMED STATEMENT AT LINE %s", p$get(2)),
                            line = as.integer(p$get(2)))
      signalCondition(m)
      p$set(1, NULL)
    },

    # Blank line
    p_statement_newline = function(doc='statement : NEWLINE', p) {
      p$set(1, NULL)
    },

    # LET statement
    p_command_let = function(doc='command : LET variable EQUALS expr', p) {
      p$set(1, list('LET', p$get(3), p$get(5)))
    },

    p_command_let_bad = function(doc='command : LET variable EQUALS error', p) {
      m <- messageCondition("BAD EXPRESSION IN LET")
      signalCondition(m)
      p$set(1, NULL)
    },

    # READ statement
    p_command_read = function(doc='command : READ varlist', p) {
      p$set(1, list('READ', p$get(3)))
    },

    p_command_read_bad = function(doc='command : READ error', p) {
      m <- messageCondition("MALFORMED VARIABLE LIST IN READ")
      signalCondition(m)
      p$set(1, NULL)
    },

    # DATA statement
    p_command_data = function(doc='command : DATA numlist', p) {
      p$set(1, list('DATA', p$get(3)))
    },

    p_command_bad = function(doc='command : DATA error', p) {
      m <- messageCondition("MALFORMED NUMBER LIST IN DATA")
      signalCondition(m)
      p$set(1, NULL)
    },

    # PRINT statement
    p_command_print = function(doc='command : PRINT plist optend', p) {
      p$set(1, list('PRINT', p$get(3), p$get(4)))
    },

    p_command_print_bad = function(doc='command : PRINT error', p) {
      m <- messageCondition("MALFORMED PRINT STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },


    # Optional ending on PRINT. Either a comma (,) or semicolon (;)
    p_optend = function(doc='optend : COMMA
                                    | SEMI
                                    |', p) {
      if (p$length() == 2) {
        p$set(1, p$get(2))
      } else {
        p$set(1, NULL)
      }
    },

    # PRINT statement with no arguments
    p_command_print_empty = function(doc='command : PRINT', p) {
      p$set(1, list('PRINT', list(), NULL))
    },

    # GOTO statement
    p_command_goto = function(doc='command : GOTO INTEGER', p) {
      p$set(1, list('GOTO', as.integer(p$get(3))))
    },

    p_command_goto_bad = function(doc='command : GOTO error', p) {
      m <- messageCondition("INVALID LINE NUMBER IN GOTO")
      signalCondition(m)
      p$set(1, NULL)
    },

    # IF-THEN statement
    p_command_if = function(doc='command : IF relexpr THEN INTEGER', p) {
      p$set(1, list('IF', p$get(3), as.integer(p$get(5))))
    },

    p_command_if_bad = function(doc='command : IF error THEN INTEGER', p) {
      m <- messageCondition("BAD RELATIONAL EXPRESSION")
      signalCondition(m)
      p$set(1, NULL)
    },

    p_command_if_bad2 = function(doc='command : IF relexpr THEN error', p) {
      m <- messageCondition("INVALID LINE NUMBER IN THEN")
      signalCondition(m)
      p$set(1, NULL)
    },

    # FOR statement
    p_command_for = function(doc='command : FOR ID EQUALS expr TO expr optstep', p) {
      p$set(1, list('FOR', p$get(3), p$get(5), p$get(7), p$get(8)))
    },

    p_command_for_bad_initial = function(doc='command : FOR ID EQUALS error TO expr optstep', p) {
      m <- messageCondition("BAD INITIAL VALUE IN FOR STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },

    p_command_for_bad_final = function(doc='command : FOR ID EQUALS expr TO error optstep', p) {
      m <- messageCondition("BAD FINAL VALUE IN FOR STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },

    p_command_for_bad_step = function(doc='command : FOR ID EQUALS expr TO expr STEP error', p) {
      m <- messageCondition("MALFORMED STEP IN FOR STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },

    # Optional STEP qualifier on FOR statement
    p_optstep = function(doc='optstep : STEP expr
                                      | empty', p) {
      if (p$length() == 3) {
        p$set(1, p$get(3))
      } else {
        p$set(1, NULL)
      }
    },

    # NEXT statement
    p_command_next = function(doc='command : NEXT ID', p) {
      p$set(1, list('NEXT', p$get(3)))
    },

    p_command_next_bad = function(doc='command : NEXT error', p) {
      m <- messageCondition("MALFORMED NEXT")
      signalCondition(m)
      p$set(1, NULL)
    },

    # END statement
    p_command_end = function(doc='command : END', p) {
      p$set(1, list('END', NULL))
    },

    # REM statement
    p_command_rem = function(doc='command : REM', p) {
      p$set(1, list('REM', p$get(2)))
    },

    # STOP statement
    p_command_stop = function(doc='command : STOP', p) {
      p$set(1, list('STOP', NULL))
    },

    # DEF statement
    p_command_def = function(doc='command : DEF ID LPAREN ID RPAREN EQUALS expr', p) {
      p$set(1, list('FUNC', p$get(3), p$get(5), p$get(8)))
    },

    p_command_def_bad_rhs = function(doc='command : DEF ID LPAREN ID RPAREN EQUALS error', p) {
      m <- messageCondition("BAD EXPRESSION IN DEF STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },

    p_command_def_bad_arg = function(doc='command : DEF ID LPAREN error RPAREN EQUALS expr', p) {
      m <- messageCondition("BAD ARGUMENT IN DEF STATEMENT")
      signalCondition(m)
      p$set(1, NULL)
    },

    # GOSUB statement
    p_command_gosub = function(doc='command : GOSUB INTEGER', p) {
      p$set(1, list('GOSUB', as.integer(p$get(3))))
    },

    p_command_gosub_bad = function(doc='command : GOSUB error', p) {
      m <- messageCondition("INVALID LINE NUMBER IN GOSUB")
      signalCondition(m)
      p$set(1, NULL)
    },

    # RETURN statement
    p_command_return = function(doc='command : RETURN', p) {
      p$set(1, list('RETURN', NULL))
    },

    # DIM statement
    p_command_dim = function(doc='command : DIM dimlist', p) {
      p$set(1, list('DIM', p$get(3)))
    },

    p_command_dim_bad = function(doc='command : DIM error', p) {
      m <- messageCondition("MALFORMED VARIABLE LIST IN DIM")
      signalCondition(m)
      p$set(1, "MALFORMED VARIABLE LIST IN DIM")
    },

    # List of variables supplied to DIM statement
    p_dimlist = function(doc='dimlist : dimlist COMMA dimitem
                                      | dimitem', p) {
      if (p$length() == 4) {
        p$set(1, c(p$get(2), p$get(4)))
      } else {
        p$set(1, p$get(2))
      }
    },

    # DIM items
    p_command_dimitem_single = function(doc='dimitem : ID LPAREN INTEGER RPAREN', p) {
      p$set(1, list(p$get(2), as.integer(p$get(4)), 0))
    },

    p_command_dimitem_double = function(doc='dimitem : ID LPAREN INTEGER COMMA INTEGER RPAREN', p) {
      p$set(1, list(p$get(2), as.integer(p$get(4)), as.integer(p$get(6))))
    },

    # Arithmetic expressions
    p_expr_binary = function(doc='expr : expr PLUS expr
                                       | expr MINUS expr
                                       | expr TIMES expr
                                       | expr DIVIDE expr
                                       | expr POWER expr', p) {
      p$set(1, list('BINOP', p$get(3), p$get(2), p$get(4)))
    },

    p_expr_number = function(doc='expr : INTEGER
                                       | FLOAT', p) {
      p$set(1, list('NUM', as.numeric(p$get(2))))
    },

    p_expr_variable = function(doc='expr : variable', p) {
      p$set(1, list('VAR', p$get(2)))
    },

    p_expr_group = function(doc='expr : LPAREN expr RPAREN', p) {
      p$set(1, list('GROUP', p$get(3)))
    },

    p_expr_unary = function(doc='expr : MINUS expr %prec UMINUS', p) {
      p$set(1, list('UNARY', '-', eval(p$get(3))))
    },

    # Relational expressions
    p_relexpr = function(doc='relexpr : expr LT expr
                                      | expr LE expr
                                      | expr GT expr
                                      | expr GE expr
                                      | expr EQUALS expr
                                      | expr NE expr', p) {
      p$set(1, list('RELOP', p$get(3), p$get(2), p$get(4)))
    },

    # Variables
    p_variable = function(doc='variable : ID
                                        | ID LPAREN expr RPAREN
                                        | ID LPAREN expr COMMA expr RPAREN', p) {
      if (p$length() == 2) {
        p$set(1, list(p$get(2), NULL, NULL))
      } else if (p$length() == 5) {
        p$set(1, list(p$get(2), p$get(4), NULL))
      } else {
        p$set(1, list(p$get(2), p$get(4), p$get(6)))
      }

    },

    # Builds a list of variable targets as an R list
    p_varlist = function(doc='varlist : varlist COMMA variable
                                      | variable', p) {
      if (p$length() > 2) {
        p$set(1, c(p$get(2), list(p$get(4))))
      } else {
        p$set(1, list(p$get(2)))
      }
    },

    # Builds a list of numbers as an R list
    p_numlist = function(doc='numlist : numlist COMMA number
                                          | number', p) {
      if (p$length() > 2) {
        p$set(1, c(p$get(2), list(p$get(4))))
      } else {
        p$set(1, list(p$get(2)))
      }
    },

    # A number. May be an integer or a float
    p_number = function(doc='number : INTEGER
                                    | FLOAT', p) {
      p$set(1, as.numeric(p$get(2)))
    },

    # A signed number.
    p_number_signed = function(doc='number : MINUS INTEGER
                                        | MINUS FLOAT', p) {
      p$set(1, as.numeric(paste0('-', p$get(3))))
    },

    # List of targets for a print statement
    # Returns a list of tuples (label,expr)
    p_plist = function(doc='plist : plist COMMA pitem
                                  | pitem', p) {
      if (p$length() > 3) {
        p$set(1, c(p$get(2), list(p$get(4))))
      } else {
        p$set(1, list(p$get(2)))
      }
    },

    p_pitem_string = function(doc='pitem : STRING', p) {
      p$set(1, list(substr(p$get(2), 2, nchar(p$get(2)) - 1), NULL))
    },

    p_item_string_expr = function(doc='pitem : STRING expr', p) {
      p$set(1, list(substr(p$get(2), 2, nchar(p$get(2)) - 1), p$get(3)))
    },

    p_item_expr = function(doc='pitem : expr', p) {
      p$set(1, list("", p$get(2)))
    },

    # Empty
    p_empty = function(doc='empty : ', p) {
    },

    # Catastrophic error handler
    p_error = function(p) {
      if(!is.null(p)) {
        m <- messageCondition("Syntax error at EOF")
        signalCondition(m)
      }
    }

  )
)


messageCondition <- function(message, line = NA, call = sys.call(-1), ...) {
  structure(
    class = c("message", "condition"),
    list(message = message, line = line, call = call, ...)
  )
}

