from infix.tokens import Token, TokenType

from atom import *


WHITESPACE  = ' \n\t'
NAMECHARS   = '.-_' 
DISJUNSTION = '|∨'
CONJUNCTION = '∧&^' 
NEGATION    = '¬~!'
TRUTHCONST  = {TruthConstant.TRUE.value, TruthConstant.FALSE.value, TruthConstant.UNKNOWN.value }

class Lexer:
    def __init__(self, text: str, atoms: dict[Atom]) -> None:
        self.text = iter(text)
        self.atoms = atoms
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
            elif self.current_char in TRUTHCONST:
                yield self.__generate_truth_const()
            elif self.current_char.isalnum():
                yield self.__parse_word()
            elif self.current_char in CONJUNCTION:
                self.advance()
                yield Token(TokenType.CONJUNCTION)
            elif self.current_char in DISJUNSTION:
                self.advance()
                yield Token(TokenType.DISJUNCTION)
            elif self.current_char in NEGATION:
                self.advance()
                yield Token(TokenType.NEGATION)
            elif self.current_char == '(':
                self.advance()
                yield Token(TokenType.LPAREN)
            elif self.current_char == ')':
                self.advance()
                yield Token(TokenType.RPAREN)
            # elif implication, reverse, equivalence 
            else:
                raise Exception(f"Lexer has found an illegal character '{self.current_char}'")
            

    
    def __parse_word(self) -> Token:
        str = self.current_char
        self.advance()
 
        while self.current_char != None and (self.current_char.isalnum() or self.current_char in NAMECHARS):
            str += self.current_char
            self.advance()
        str_lower = str.lower()
        if str_lower == 'not':
            return Token(TokenType.NEGATION)
        elif str_lower == 'and':
            return Token(TokenType.CONJUNCTION)
        elif str_lower == 'or':
            return Token(TokenType.DISJUNCTION)
        elif str_lower == 't':
            return Token(TokenType.TRUTHCONST, TruthConstant.TRUE)
        elif str_lower == 'f':
            return Token(TokenType.TRUTHCONST, TruthConstant.FALSE)
        elif str_lower == 'u':
            return Token(TokenType.TRUTHCONST, TruthConstant.UNKNOWN)

        
        if str not in self.atoms:
            self.atoms[str] = Atom(str)
        return Token(TokenType.ATOM, self.atoms[str])

    def __generate_truth_const(self) -> Token:
        truth_const_str = self.current_char
        self.advance()
        if truth_const_str == TruthConstant.FALSE.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.FALSE)
        elif truth_const_str == TruthConstant.TRUE.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.TRUE)
        elif truth_const_str == TruthConstant.UNKNOWN.value:
            return Token(TokenType.TRUTHCONST, TruthConstant.UNKNOWN)