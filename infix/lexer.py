from infix.tokens import Token, TokenType

from atom import *


WHITESPACE  = ' \n\t'
NAMECHARS   = '.-_' 
CONJUNCTION = '∨'
DISJUNSTION = '∧' 
NEGATION    = '¬'

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
            elif self.current_char.isalnum():
                yield self.__generate_atom()
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
            

    
    def __generate_atom(self) -> Token:
        atom_str = self.current_char
        self.advance()
 
        while self.current_char != None and (self.current_char.isalnum() or self.current_char in NAMECHARS):
            atom_str += self.current_char
            self.advance()
        if atom_str not in self.atoms:
            self.atoms[atom_str] = Atom(atom_str)
        return Token(TokenType.ATOM, self.atoms[atom_str])