# from program import Program
# from atom import *
# from truth_tables import *
# from clauses import *
# from interpretation import *

from pyparsing import infixNotation, opAssoc, Keyword, Word, alphas, ParserElement

ParserElement.enablePackrat()

# define classes to be built at parse time, as each matching
# expression type is parsed
class BoolOperand:
    def __init__(self, t):
        self.label = t[0]
        self.value = eval(t[0])

    def __bool__(self):
        return self.value

    def __str__(self):
        return self.label

    __repr__ = __str__


class BoolBinOp:
    def __init__(self, t):
        self.args = t[0][0::2]

    def __str__(self):
        sep = " %s " % self.reprsymbol
        return "(" + sep.join(map(str, self.args)) + ")"

    def __bool__(self):
        return self.evalop(bool(a) for a in self.args)

    __nonzero__ = __bool__


class BoolAnd(BoolBinOp):
    reprsymbol = "&"
    evalop = all


class BoolOr(BoolBinOp):
    reprsymbol = "|"
    evalop = any


class BoolNot:
    def __init__(self, t):
        self.arg = t[0][1]

    def __bool__(self):
        v = bool(self.arg)
        return not v

    def __str__(self):
        return "~" + str(self.arg)

    __repr__ = __str__


TRUE = Keyword("True")
FALSE = Keyword("False")
boolOperand = TRUE | FALSE | Word(alphas, max=1)
boolOperand.setParseAction(BoolOperand)

# define expression, based on expression operand and
# list of operations in precedence order
boolExpr = infixNotation(
    boolOperand,
    [
        ("not", 1, opAssoc.RIGHT, BoolNot),
        ("and", 2, opAssoc.LEFT, BoolAnd),
        ("or", 2, opAssoc.LEFT, BoolOr),
    ],
)
