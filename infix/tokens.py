from enum import Enum
from dataclasses import dataclass

class TokenType(Enum):
   ATOM         = 0
   CONJUNCTION  = 1
   DISJUNCTION  = 2
   NEGATION     = 3
   LPAREN       = 4
   RPAREN       = 5
   TRUTHCONST   = 6
#    IMPLICATION  = 7
#    REVERSE      = 8
#    EQUIVALENCE  = 9

@dataclass
class Token:
    type: TokenType
    value: any = None

    def __repr__(self):
        return self.type.name + (f":{self.value}" if self.value != None else "")
