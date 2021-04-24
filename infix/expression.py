from infix.interpreter import Interpreter
from truth_constant import TruthConstant
from infix.parser_ import Parser
from infix.lexer import Lexer
from typing import Any, Iterator
from atom import Atom
class InfixExpression:
    def __init__(self, expression: str, atoms: dict[str, Atom]) -> None:
        self.expression = expression.replace('&', '∧').replace('^', '∧').replace('~', '¬').replace('!', '¬').replace('|', '∨')
        self.atoms = atoms
        self.__parse_lexer_tokens(self.__get_lexer_tokens())

        


    def __get_lexer_tokens(self) -> Iterator:
        return Lexer(self.expression, self.atoms).generate_tokens()

    def __parse_lexer_tokens(self, tokens:Iterator) -> Any:
        return Parser(tokens).parse()

    def __interpret(self, tree) -> TruthConstant:
        return Interpreter().visit(tree)
    
    def evaluate(self) -> TruthConstant:
        return self.__interpret(self.__parse_lexer_tokens(self.__get_lexer_tokens()))