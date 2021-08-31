
from infix.interpreter import Interpreter
from truth_constant import TruthConstant
from infix.parser_ import Parser
from infix.lexer import Lexer
from typing import Any, Iterator
from infix.tokens import Token, TokenType
from atom import Atom
class InfixExpression:
    def __init__(self, expression: str, ground_terms: dict[str, TruthConstant]):
        self.expression = expression
        self.ground_terms = ground_terms
        self.node_string = str(self.parse_lexer_tokens(self.get_lexer_tokens())) # string representation of the expression
        
        

    # def bold_abnormalities(self) -> str:
    #     '''
    #         Analogous to node_string ( __repr__ ), except here, abnormality node tokens are replaced with dummy clones whose names have markdown boldness
    #     '''

    #     mod_tokens = list(self.get_lexer_tokens())
    #     for i in range(len(mod_tokens)):
    #         token = mod_tokens[i]
    #         if token.type == TokenType.ATOM and token.value.is_abnormality:
    #             mod_tokens[i] = Token(TokenType.ATOM, Atom(f"**{token.value.name}**", token.value.ground_value, True))
    #     return str(self.parse_lexer_tokens(mod_tokens))

    def replace_var(self, original:str, replacement:str) -> str:
        str = ''
        for token in self.get_lexer_tokens():
            if token.type == TokenType.VAR and token.value == original:
                str = str + replacement
            elif token.type == TokenType.COMMA:
                str = str + ','
            elif token.type == TokenType.CONJUNCTION:
                str = str + '∧'
            elif token.type == TokenType.DISJUNCTION:
                str = str + '∨'
            elif token.type == TokenType.NEGATION:
                str = str + '~'
            elif token.type in [TokenType.ORDER_LPAREN, TokenType.PRED_LPAREN] :
                str = str + '('
            elif token.type in [TokenType.ORDER_RPAREN, TokenType.PRED_RPAREN] :
                str = str + ')'
            elif token.type in [TokenType.PREDICATE, TokenType.ATOM, TokenType.CONST]:
                str = str + f' {token.value} '  
            elif token.type == TokenType.TRUTHCONST:
                str = str + token.value.value
        return str   

    def is_ground(self) -> bool:
        for token in self.get_lexer_tokens():
            if token.type == TokenType.VAR:
                return False
        return True
    def __repr__(self) -> str:
        return self.node_string

    def get_lexer_tokens(self) -> list:
        return Lexer(self.expression).generate_tokens()

    def parse_lexer_tokens(self, tokens:Iterator) -> Any:
        return Parser(tokens, self.ground_terms).parse()

    def __interpret(self, tree) -> TruthConstant:
        return Interpreter().visit(tree, self.ground_terms)
    
    def evaluate(self) -> TruthConstant:
        return self.__interpret(self.parse_lexer_tokens(self.get_lexer_tokens()))
    
    def disjoin(self, expr:'InfixExpression') -> 'InfixExpression':
        '''
        Append an infix expression by disjunction of another infix expression. Ground value dictionaries must match by identity (is) 
        '''

        if self.ground_terms is expr.ground_terms:
            return InfixExpression(f'({self.expression}) ∨ ({expr.expression})', self.ground_terms)
        else:
            raise Exception('Could not append to InfixExpression: ground value dictionaries do not match')

    def __eq__(self, other):
        if self.node_string == other.node_string and self.ground_terms is other.ground_terms:
            return True
        else:
            return False
    
    def __hash__(self):
        return hash(self.expression)