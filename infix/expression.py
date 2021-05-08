from infix.tokens import TokenType
from infix.interpreter import Interpreter
from truth_constant import TruthConstant
from infix.parser_ import Parser
from infix.lexer import Lexer
from typing import Any, Iterator
from atom import Atom
class InfixExpression:
    def __init__(self, expression: str, atoms: dict[str, Atom]) -> None:
        self.expression = expression
        self.atoms = atoms # reference set of atoms
        self.token_string = str(self.__parse_lexer_tokens(self.__get_lexer_tokens()))
        self.atoms_here = set()
        for token in self.__get_lexer_tokens():
            if token.type == TokenType.ATOM:
                self.atoms_here.add(token.value)
                



        

    def __repr__(self) -> str:
        return self.token_string

    def __get_lexer_tokens(self) -> Iterator:
        return Lexer(self.expression, self.atoms).generate_tokens()

    def __parse_lexer_tokens(self, tokens:Iterator) -> Any:
        return Parser(tokens).parse()

    def __interpret(self, tree) -> TruthConstant:
        return Interpreter().visit(tree)
    
    def evaluate(self) -> TruthConstant:
        return self.__interpret(self.__parse_lexer_tokens(self.__get_lexer_tokens()))
    
    def disjoin(self, expr:'InfixExpression') -> 'InfixExpression':
        '''
        Append an infix expression by disjunction of another infix expression. Atom sets must match by identity (is) 
        '''

        if self.atoms is expr.atoms:
            return InfixExpression(f'({self.expression}) ∨ ({expr.expression})', self.atoms)
        else:
            raise Exception('Could not append to InfixExpression: sets of atoms do not match')

    def __eq__(self, other):
        if self.token_string == other.token_string and self.atoms is other.atoms:
            return True
        else:
            return False
    
    def __hash__(self):
        return hash(self.expression)