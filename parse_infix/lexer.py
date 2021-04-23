from parse_infix.tokens import Token, TokenType

from atom import *


WHITESPACE = ' \n\t'

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
            elif self.current_char == '^' or self.current_char == '∧' or self.current_char == '&':
                self.advance()
                yield Token(TokenType.CONJUNCTION)
            elif self.current_char == '∨' or self.current_char == '|':
                self.advance()
                yield Token(TokenType.CONJUNCTION)
            elif self.current_char == '(':
                self.advance()
                yield Token(TokenType.LPAREN)
            elif self.current_char == ')':
                self.advance()
                yield Token(TokenType.LPAREN)
            # elif implication, reverse, equivalence 
            else:
                raise Exception(f"Illegal character '{self.current_char}'")
            

    
    def __generate_atom(self) -> Token:
        atom_str = self.current_char
        self.advance()
 
        while self.current_char != None and self.current_char.isalnum():
            atom_str += self.current_char
            self.advance()
        if atom_str not in self.atoms:
            self.atoms[atom_str] = Atom(atom_str)
        return Token(TokenType.ATOM, self.atoms[atom_str])