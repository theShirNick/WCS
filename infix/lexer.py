from collections import deque
from infix.tokens import Token, TokenType

from atom import *


WHITESPACE  = ' \n\t'
NAMECHARS   = '.-_' 
DISJUNSTION = '|∨'
CONJUNCTION = '∧&^' 
NEGATION    = '¬~!'
TRUTHCONST  = {TruthConstant.TRUE.value, TruthConstant.FALSE.value, TruthConstant.UNKNOWN.value }
CONDITIONAL_CLASSIFIER = '*'

class Lexer:
    def __init__(self, text: str) -> None:
        self.text = iter(text)
        self.token_sequence = list()
        self.bracket_stack = deque()
        self.advance()

    def advance(self):
        try:
            self.current_char = next(self.text)
        except StopIteration:
            self.current_char = None

    def generate_tokens(self):
        while self.current_char != None:
            if self.current_char in WHITESPACE:
                self.advance()
            elif self.current_char in CONDITIONAL_CLASSIFIER:
                self.advance()
            elif self.current_char == '<': # ignore HTML
                while self.current_char != '>':
                    self.advance()
                self.advance()
            elif self.current_char in TRUTHCONST:
                self.token_sequence.append(self.__generate_truth_const())
            elif self.current_char.isalnum():
                self.token_sequence.append(self.__parse_word())
                if len(self.token_sequence) > 1 and self.token_sequence[-2].type == TokenType.TBD and self.token_sequence[-1].type == TokenType.TBD:
                    self.token_sequence[-2].type = TokenType.PREDICATE
                    self.token_sequence[-1].type = TokenType.CONST
                elif len(self.token_sequence) > 1 and self.token_sequence[-2].type == TokenType.TBD and self.token_sequence[-1].type == TokenType.VAR:
                    self.token_sequence[-2].type = TokenType.PREDICATE
                elif len(self.token_sequence) > 1 and self.token_sequence[-2].type == TokenType.PRED_LPAREN and self.token_sequence[-1].type == TokenType.TBD:
                    self.token_sequence[-1].type = TokenType.CONST
            elif self.current_char in CONJUNCTION:
                self.advance()
                self.token_sequence.append(Token(TokenType.CONJUNCTION))
            elif self.current_char in DISJUNSTION:
                self.advance()
                self.token_sequence.append(Token(TokenType.DISJUNCTION))
            elif self.current_char in NEGATION:
                self.advance()
                self.token_sequence.append(Token(TokenType.NEGATION))
            elif self.current_char == '(':
                self.advance()
                if len(self.token_sequence) > 0 and self.token_sequence[-1].type in [TokenType.TBD, TokenType.PREDICATE]:
                    self.token_sequence[-1].type = TokenType.PREDICATE
                    self.token_sequence.append(Token(TokenType.PRED_LPAREN))
                    self.bracket_stack.append(TokenType.PRED_LPAREN)
                else:
                    self.token_sequence.append(Token(TokenType.ORDER_LPAREN))
                    self.bracket_stack.append(TokenType.ORDER_LPAREN)
            elif self.current_char == ')':
                self.advance()
                if len(self.bracket_stack) == 0:
                     raise Exception(f"Number of closing brackets do not match the number of opened brackets")
                
                if len(self.token_sequence) > 0 and self.token_sequence[-1].type in [TokenType.VAR, TokenType.CONST]:
                    if self.bracket_stack[-1] == TokenType.PRED_LPAREN:
                        self.token_sequence.append(Token(TokenType.PRED_RPAREN))
                    if self.bracket_stack[-1] == TokenType.ORDER_LPAREN:
                        self.token_sequence.append(Token(TokenType.ORDER_RPAREN))
                    self.bracket_stack.pop()
                elif len(self.token_sequence) > 0 and  len(self.bracket_stack) > 0 and self.bracket_stack[-1] == TokenType.PRED_LPAREN:
                    self.token_sequence.append(Token(TokenType.PRED_RPAREN))
                    self.token_sequence[-2].type = TokenType.CONST
                    self.bracket_stack.pop()
                else:
                    self.token_sequence.append(Token(TokenType.ORDER_RPAREN))
                    self.bracket_stack.pop() 
            elif self.current_char == ',' and len(self.token_sequence) > 0:
                self.advance()
                if self.token_sequence[-1].type == TokenType.TBD:
                    self.token_sequence[-1].type = TokenType.CONST
                    self.token_sequence.append(Token(TokenType.COMMA)) 
                elif self.token_sequence[-1].type in [TokenType.CONST, TokenType.VAR]:
                    self.token_sequence.append(Token(TokenType.COMMA))
            else:
                raise Exception(f"Lexer has found an illegal character '{self.current_char}'")

        for token in self.token_sequence:
            if token.type == TokenType.TBD:
                token.type = TokenType.ATOM
        return self.token_sequence    

    
    def __parse_word(self) -> Token:
        str = self.current_char
        self.advance()
 
        while self.current_char != None and (self.current_char.isalnum() or self.current_char in NAMECHARS):
            str += self.current_char
            self.advance()
        if  str.lower() == 'not':
            return Token(TokenType.NEGATION)
        elif  str.lower() == 'and':
            return Token(TokenType.CONJUNCTION)
        elif  str.lower() == 'or':
            return Token(TokenType.DISJUNCTION)
        elif str == 'T':
            return Token(TokenType.TRUTHCONST, TruthConstant.TRUE)
        elif str == 'F':
            return Token(TokenType.TRUTHCONST, TruthConstant.FALSE)
        elif str == 'U':
            return Token(TokenType.TRUTHCONST, TruthConstant.UNKNOWN)

        if str[0].isupper():
            return Token(TokenType.VAR, f'<font color="aqua">{str}</font>')

        return Token(TokenType.TBD, str)

    def __generate_truth_const(self) -> Token:
        truth_const_str = self.current_char
        self.advance()
        if truth_const_str == TruthConstant.FALSE.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.FALSE)
        elif truth_const_str == TruthConstant.TRUE.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.TRUE)
        elif truth_const_str == TruthConstant.UNKNOWN.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.UNKNOWN)