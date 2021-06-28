from infix.nodes import AtomNode
from infix.tokens import Token, TokenType
from infix.interpreter import Interpreter
from truth_constant import TruthConstant
from infix.parser_ import Parser
from infix.lexer import Lexer
from typing import Any, Iterator
from atom import Atom
class InfixExpression:
    def __init__(self, expression: str, atoms: dict[str, Atom]):
        self.expression = expression
        self.atoms = atoms # reference set of atoms
        self.node_string = str(self.parse_lexer_tokens(self.get_lexer_tokens())) # string representation of the exprassion
        self.atoms_here = set()  

        for token in self.get_lexer_tokens():
            if token.type == TokenType.ATOM:
                self.atoms_here.add(token.value)

    def bold_abnormalities(self) -> str:
        '''
            Analogous to node_string, except here, abnormality node tokens are replaced with dummy clones whose names have markdown boldness
        '''

        mod_tokens = list(self.get_lexer_tokens())
        for i in range(len(mod_tokens)):
            token = mod_tokens[i]
            if token.type == TokenType.ATOM and token.value.is_abnormality:
                mod_tokens[i] = Token(TokenType.ATOM, Atom(f"**{token.value.name}**", token.value.ground_value, True))
        return str(self.parse_lexer_tokens(mod_tokens))

    def __repr__(self) -> str:
        return self.node_string

    def get_lexer_tokens(self) -> Iterator:
        return Lexer(self.expression, self.atoms).generate_tokens()

    def parse_lexer_tokens(self, tokens:Iterator) -> Any:
        return Parser(tokens).parse()

    def __interpret(self, tree) -> TruthConstant:
        return Interpreter().visit(tree)
    
    def evaluate(self) -> TruthConstant:
        return self.__interpret(self.parse_lexer_tokens(self.get_lexer_tokens()))
    
    def disjoin(self, expr:'InfixExpression') -> 'InfixExpression':
        '''
        Append an infix expression by disjunction of another infix expression. Atom sets must match by identity (is) 
        '''

        if self.atoms is expr.atoms:
            return InfixExpression(f'({self.expression}) ∨ ({expr.expression})', self.atoms)
        else:
            raise Exception('Could not append to InfixExpression: sets of atoms do not match')

    def __eq__(self, other):
        if self.node_string == other.node_string and self.atoms is other.atoms:
            return True
        else:
            return False
    
    def __hash__(self):
        return hash(self.expression)