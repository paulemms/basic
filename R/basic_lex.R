# An implementation of Dartmouth BASIC (1964)

keywords <- c(
    'LET', 'READ', 'DATA', 'PRINT', 'GOTO', 'IF', 'THEN', 'FOR', 'NEXT', 'TO', 'STEP',
    'END', 'STOP', 'DEF', 'GOSUB', 'DIM', 'REM', 'RETURN', 'RUN', 'LIST', 'NEW'
)

TOKENS <- c(keywords,
    'EQUALS', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER',
    'LPAREN', 'RPAREN', 'LT', 'LE', 'GT', 'GE', 'NE',
    'COMMA', 'SEMI', 'INTEGER', 'FLOAT', 'STRING',
    'ID', 'NEWLINE'
)

BasicLexer <- R6::R6Class(

  classname = "BasicLexer",

  public = list(

    tokens = TOKENS,

    t_ignore = ' \t',

    t_REM = function(re='REM .*', t) {
      return(t)
    },

    t_ID = function(re='[A-Z][A-Z0-9]*', t) {
      if (t$value %in% keywords) t$type <- t$value
      return(t)
    },

    t_EQUALS = function(re='\\=', t) t,
    t_PLUS = function(re='\\+', t) t,
    t_MINUS = function(re='\\-', t) t,
    t_TIMES = function(re='\\*', t) t,
    t_POWER = function(re='\\^', t) t,
    t_DIVIDE = function(re='\\/', t) t,
    t_LPAREN = function(re='\\(', t) t,
    t_RPAREN = function(re='\\)', t) t,
    t_LT = function(re='\\<\\=|(\\<)', t) t,
    t_LE = function(re='\\<\\=', t) t,
    t_GT = function(re='\\>\\=|(\\>)', t) t,
    t_GE = function(re='\\>\\=', t) t,
    t_NE = function(re='\\<\\>', t) t,
    t_COMMA = function(re='\\,', t) t,
    t_SEMI = function(re='\\;', t) t,
    t_INTEGER = function(re='\\d+\\.\\d+|(\\d+)', t) t,
    t_FLOAT = function(re='((\\d*\\.\\d+)(E[\\+-]?\\d+)?|([1-9]\\d*E[\\+-]?\\d+))', t) t,
    t_STRING = function(re='\\".*?\\"', t) t,

    t_NEWLINE = function(re='\\n', t) {
      t$lexer$lineno = t$lexer$lineno + 1
      return(t)
    },

    t_error = function(t) {
      w <- warningCondition(sprintf("Illegal character %s\n", t$value[1]))
      warning(w)
      t$lexer$skip(1)
      return(t)
    }

  )
)

