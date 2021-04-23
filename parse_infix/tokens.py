from enum import Enum
from dataclasses import dataclass

class TokenType(Enum):
   ATOM         = 0
   CONJUNCTION  = 1
   DISJUNCTION  = 2
   NEGATION     = 3
   LPAREN       = 4
   RPAREN       = 5
#    IMPLICATION  = 6
#    REVERSE      = 7
#    EQUIVALENCE  = 8

@dataclass
class Token:
    type: TokenType
    value: any = None

    def __repr__(self):
        return self.type.name + (f":{self.value}" if self.value != None else "")
