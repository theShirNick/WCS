from enum import Enum
from dataclasses import dataclass

class TokenType(Enum):
   ATOM             = 0
   CONJUNCTION      = 1
   DISJUNCTION      = 2
   NEGATION         = 3
   ORDER_LPAREN     = 4
   ORDER_RPAREN     = 5
   TRUTHCONST       = 6
   PREDICATE        = 7
   PRED_LPAREN      = 8
   PRED_RPAREN      = 9
   VAR              = 10
   CONST            = 11
   COMMA            = 12
   CONTEXT          = 13

   TBD              = 99


@dataclass
class Token:
    type: TokenType
    value: any = None

    def __repr__(self):
        return self.type.name + (f":{self.value}" if self.value != None else "")
