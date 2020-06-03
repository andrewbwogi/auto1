#lang brag

regex  : union | simple
union  : regex "|" simple
simple : concat | basic
concat : simple basic
basic  : star | plus | q | elem
star   : elem "*"
plus   : elem "+"
q      : elem "?"
elem   : group | dot | char
char   : CHAR-TOK
dot    : "."
group  : "(" regex ")"