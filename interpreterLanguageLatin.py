# -----------------------------------------------------------------------------
# interpreterLanguageLatin.py
#
# An interpreter of a language in Latin.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Basic : if(expression_bool) {block} else if(expression_bool) {block} else {bloc}
# Our : si expression_bool {block} aliud si expression_bool {block} aliud {bloc}
#
# Basic : while(expression_bool) {bloc}
# Our : dum(expression_bool) {bloc}
#
# Basic : for(statement_assign, expression_bool, expression_binop) {bloc}
# Our : quia(statement_assign, expression_bool, expression_binop) {bloc}
#
# Basic : do{bloc} while(expression_bool)
# Our : fac{bloc} dum(expression_bool)
#
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'bloc': 'BLOC',
    'si':   'IF',
    'aliud':'ELSE',
    'dum':  'WHILE',
    'quia': 'FOR',
    'fac':  'DO'
}

tokens = [
    'NUMBER', 'EQUAL',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO',
    'EQUALITY', 'NON_EQUALITY', 'LESSTHAN_AND_EQUALITY', 'GREATERTHAN_AND_EQUALITY', 'LESSTHAN', 'GREATERTHAN',
    'LPAREN', 'RPAREN', 'SEMICOLON', 'LBRACKET', 'RBRACKET',
    'NAME'
] + list(reserved.values())

# Tokens

t_EQUAL = r'='

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MODULO = r'%'

t_EQUALITY = r'=='
t_NON_EQUALITY = r'!='

t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_LESSTHAN_AND_EQUALITY = r'<='
t_GREATERTHAN_AND_EQUALITY = r'>='

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


def t_reserved(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')  # Check for reserved words
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
# nonassoc : do not allowed to put many of this token on the same sentence
precedence = (
    ('nonassoc', 'EQUALITY', 'NON_EQUALITY', 'LESSTHAN', 'GREATERTHAN', 'LESSTHAN_AND_EQUALITY', 'GREATERTHAN_AND_EQUALITY'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)

# dictionary of names (for storing variables)
names = {}


"""def p_master(p):
    '''master : bloc'''
    p[0] = p[1]
    print(p[0])
    eval(p[0])"""


def p_bloc(p):
    '''bloc : bloc statement
            | statement '''

    if len(p) == 3:
        p[0] = ('bloc', p[1], p[2])
    else:
        p[0] = p[1]

    print(eval(p[0]))


"""def p_statement_condition(p):
    '''statement : IF expression body
                 | IF expression body ELSE body'''
                #| IF expression body ELSE statement

    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    elif len(p) == 6:
        p[0] = (p[1], p[2], p[3], (p[4], p[5]))
"""


def p_statement_condition(p):
    '''statement : IF expression LBRACKET bloc RBRACKET
                 | IF expression LBRACKET bloc RBRACKET ELSE LBRACKET bloc RBRACKET'''
                #| IF expression body ELSE statement

    if len(p) == 6:
        p[0] = (p[1], p[2], p[4])
    elif len(p) == 10:
        p[0] = (p[1], p[2], p[4], (p[6], p[8]))


def p_body(p):
    'body : LBRACKET bloc RBRACKET'

    p[0] = p[2]

    # print("body")


def p_statement_loop(p):
    '''statement : WHILE expression body
                 | FOR NAME EQUAL expression SEMICOLON expression SEMICOLON expression body'''

    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    elif len(p) == 10:
        p[0] = (p[1], p_statement_assign([p[2], p[3], p[4], p[5]]), p[6], p[7], p[8], p[9])


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
                  | expression DIVIDE expression
                  | expression MODULO expression'''
    if p[2] == '+':
        p[0] = ('+', p[1], p[3])
    elif p[2] == '-':
        p[0] = ('-', p[1], p[3])
    elif p[2] == '*':
        p[0] = ('*', p[1], p[3])
    elif p[2] == '/':
        p[0] = ('/', p[1], p[3])
    elif p[2] == '%':
        p[0] = ('%', p[1], p[3])


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
        elif p[0] == '%':
            return eval(p[1]) % eval(p[2])

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
            #print("ev:", a)
            names[a] = eval(p[2])
            #print("ev:", names[a])
            #print("ev:", names.get(a))
            return names.get(a)

        elif p[0] == 'bloc':
            eval(p[1])

# @TODO left one bug, the result of the block is not display

        elif p[0] == 'si':
            # print("[SI]")
            tmp = eval(p[1])

            if tmp:
                return eval(p[2])
            elif len(p) == 4:
                # print("[FALSE]")
                eval(p[3])
        elif p[0] == 'aliud':
            return eval(p[1])

# @TODO loop statement

        elif p[0] == 'dum':
            tmp = eval(p[1])
            while (tmp is True) or (tmp > 0):
                eval(p[2])
                tmp = eval(p[1])
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
        #print(names)
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    print("Syntax error at '%s'" % p.value)


yacc.yacc()

"""while True:
    try:
        s = input('calcLatin > ')  # use input() on Python 3
    except EOFError:
        break"""

with open("code.txt") as f:
    s = f.read()

yacc.parse(s)
