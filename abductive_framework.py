from infix.expression import InfixExpression
from truth_constant import TruthConstant
from truth_tables import reverse

class Abducible():

    def __init__(self, body: InfixExpression, head: InfixExpression):
        '''
        body ← head

        An abducible is a fact A ← ⊤ or A ← ⊥ for an undefined A 
        '''
        if len(body.atoms_here) == 1: 
            self.body = body
        else:
            raise Exception(f"An abducible must have 1 atom in the body; found {len(body.atoms_here)}")
        
        if head.token_string == str(TruthConstant.TRUE.value) or head.token_string == str(TruthConstant.FALSE.value):
            self.head = head
        else:
            raise Exception(f"An abducible's head must be ⊤ or ⊥; found {head.token_string}")
    
    def __repr__(self):
        return f"{self.body} ← {self.head}"

    def __eq__(self, other):
        if self.body.token_string == other.body.token_string and self.head.token_string is other.head.token_string:
            return True
        else:
            return False
    
    def __hash__(self):
        return hash(self.body.token_string + self.head.token_string)

    def evaluate(self) -> TruthConstant:

        return reverse(self.body.evaluate(), self.head.evaluate())

