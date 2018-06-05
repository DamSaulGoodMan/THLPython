# -----------------------------------------------------------------------------
# interpreterLanguageLatin.py
#
# An interpreter of a language in Latin.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Basic : if(expression_bool) {block} else if(expression_bool) {block} else {block}
# Our : si expression_bool {block} si_igitur expression_bool {block} aliud {block}
#
# Basic : while(expression_bool) {block}
# Our : dum(expression_bool) {block}
#
# Basic : for(statement_assign, expression_bool, expression_binop) {block}
# Our : quia(statement_assign, expression_bool, expression_binop) {block}
#
# Basic : do{block} while(expression_bool)
# Our : fac{block} dum(expression_bool)
#
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc

# reserved = {
#    'si': 'IF',
#    'aliud': 'ELSE',
#    'sinaliter': 'ELSEIF'
# }

tokens = [
    'NUMBER', 'EQUAL',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EQUALITY', 'NON_EQUALITY', 'LESSTHAN_AND_EQUALITY', 'GREATERTHAN_AND_EQUALITY', 'LESSTHAN', 'GREATERTHAN', 'IF', 'ELSE', 'ELSEIF',
    'LPAREN', 'RPAREN', 'SEMICOLON', 'LBRACKET', 'RBRACKET',
    'NAME'
]
         # + list(reserved.values())

# Tokens

t_EQUAL = r'='

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'

t_EQUALITY = r'=='
t_NON_EQUALITY = r'!='

t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_LESSTHAN_AND_EQUALITY = r'<='
t_GREATERTHAN_AND_EQUALITY = r'>='

t_IF = r'IF'
t_ELSE = r'ELSE'
t_ELSEIF = r'ELSEIF'

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_LBRACKET = r'\{'
t_RBRACKET = r'\}'

# t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'

# Ignored characters
t_ignore = " \t"


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # t.type = reserved.get(t.value, 'ID')    # Check for reserved words
    return t


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
    ('left', 'IF', 'ELSE', 'ELSEIF'),
    ('left', 'EQUALITY', 'NON_EQUALITY', 'LESSTHAN', 'GREATERTHAN', 'LESSTHAN_AND_EQUALITY', 'GREATERTHAN_AND_EQUALITY'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)

# dictionary of names (for storing variables)
names = {}


def p_bloc(p):
    '''bloc : bloc statement
            | statement '''

    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[1]

    print(eval(p[0]))


def p_statement_if(p):
    '''statement : IF expression LBRACKET bloc RBRACKET
                 | IF expression LBRACKET bloc RBRACKET else'''

    #if len(p) == 4:
    p[0] = p[1]


# def p_body(p):
#     'body : LBRACKET bloc RBRACKET'
#
#     p[0] = p[2]
#
#     # print("body")


def p_else(p):
    '''else : ELSE expression LBRACKET bloc RBRACKET
            | ELSEIF expression expression LBRACKET bloc RBRACKET else'''

    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[4]


def p_statement_assign(p):
    'statement : NAME EQUAL expression SEMICOLON'

    p[0] = ('=', p[1], p[3])
    # print(eval(p[0]))


def p_statement_expr(p):
    'statement : expression SEMICOLON'

    p[0] = p[1]
    # print(eval(p[1]))


def p_expression_bool(p):
    '''expression : expression EQUALITY expression
                  | expression NON_EQUALITY expression
                  | expression LESSTHAN expression
                  | expression GREATERTHAN expression
                  | expression LESSTHAN_AND_EQUALITY expression
                  | expression GREATERTHAN_AND_EQUALITY expression'''

    if p[2] == '==':
        p[0] = ('==', p[1], p[3])
    elif p[2] == '!=':
        p[0] = ('!=', p[1], p[3])
    elif p[2] == '<':
        p[0] = ('<', p[1], p[3])
    elif p[2] == '>':
        p[0] = ('>', p[1], p[3])
    elif p[2] == '<=':
        p[0] = ('<=', p[1], p[3])
    elif p[2] == '>=':
        p[0] = ('>=', p[1], p[3])
    # print(eval(p[0]))


def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if p[2] == '+':
        p[0] = ('+', p[1], p[3])
    elif p[2] == '-':
        p[0] = ('-', p[1], p[3])
    elif p[2] == '*':
        p[0] = ('*', p[1], p[3])
    elif p[2] == '/':
        p[0] = ('/', p[1], p[3])
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
        elif p[0] == '<':
            return eval(p[1]) < eval(p[2])
        elif p[0] == '>':
            return eval(p[1]) > eval(p[2])
        elif p[0] == '<=':
            return eval(p[1]) <= eval(p[2])
        elif p[0] == '>=':
            return eval(p[1]) >= eval(p[2])
        elif p[0] == '=':
            a = eval(p[1])
            names[a] = eval(p[2])
            return names.get(a)
        elif p[0] == 'si':
            print(p[1])
            print(p[2])
            print(p[3])
            if eval(p[1]) <= 0:
                return eval(p[2])
            else:
                return eval(p[3])
        elif p[0] == 'aliud':
            return eval(p[1])

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
    print("name")
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
        s = input('calcLatin > ')  # use input() on Python 3
    except EOFError:
        break
    yacc.parse(s)
