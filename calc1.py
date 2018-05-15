# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc

tokens = (
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQUALITY', 'NON_EQUALITY',
    'LPAREN', 'RPAREN', 'SEMICOLON',
)

# Tokens


t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_SEMICOLON = r';'
t_EQUALITY = '=='
t_NON_EQUALITY = '!='
# t_EQUAL = r'='


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
lex.lex()

# Precedence rules for the arithmetic operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'EQUALITY', 'NON_EQUALITY'),
    ('right', 'UMINUS')
)

# dictionary of names (for storing variables)
names = {}


def p_bloc(p):
    '''bloc : statement bloc
     | statement '''


def p_statement_expr(p):
    '''statement : expression SEMICOLON
                 | expression'''
    print(eval(p[1]))


def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression EQUALITY expression
                  | expression NON_EQUALITY expression'''
    if p[2] == '+':
        p[0] = ('+', p[1], p[3])
    elif p[2] == '-':
        p[0] = ('-', p[1], p[3])
    elif p[2] == '*':
        p[0] = ('*', p[1], p[3])
    elif p[2] == '/':
        p[0] = ('/', p[1], p[3])
    elif p[2] == '==':
        p[0] = ('==', p[1], p[3])
    elif p[2] == '!=':
        p[0] = ('!=', p[1], p[3])
    # eval(p[0])


def eval(p):
    print(p)
    if type(p) == tuple:
        if p[0] == '+':
            return eval(p[1]) + eval(p[2])
        elif p[0] == '-':
            return eval(p[1]) - eval(p[2])
        elif p[0] == '*':
            return eval(p[1]) * eval(p[2])
        elif p[0] == '/':
            return eval(p[1]) / eval(p[2])
        elif p[0] == '==':
            return eval(p[1]) == eval(p[2])
        elif p[0] == '!=':
            return eval(p[1]) != eval(p[2])
    else:
        return p


def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]


def p_expression_name(p):
    'expression : NAME'
    try:
        p[0] = names[p[1]]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    print("Syntax error at '%s'" % p.value)


yacc.yacc()

while True:
    try:
        s = input('calc1 > ')  # use input() on Python 3
    except EOFError:
        break
    yacc.parse(s)
