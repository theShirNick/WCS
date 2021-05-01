from abc import ABC, abstractmethod
from truth_constant import TruthConstant
from truth_tables import equivalence, reverse
from infix.expression import InfixExpression

# TODO don't like the structure of Clause, WC_Clause and weird incomplete inhereitance of get_equv_clause()
class Clause(ABC):

    @abstractmethod
    def __str__(self):
        pass

    @abstractmethod
    def evaluate() -> TruthConstant:
        pass

    def get_equv_clause(self):
        '''
            Get a Weakly Completed instance of this Clause 
        '''

        return self

class Rule(Clause):

    def __init__(self, body: InfixExpression, head: InfixExpression):
        '''
        body ← head

        "if head then body" or "body if head"
        '''

        self.head = head
        self.body = body
    
    def __str__(self):
        return f"{self.body} ← {self.head}"

    def evaluate(self) -> TruthConstant:

        return reverse(self.body.evaluate(), self.head.evaluate())
    
    def get_equv_clause(self):
        return WC_Rule(self.body, self.head)

### Weakly Completed Clauses

class WC_Rule(Clause):

    def __init__(self, body: InfixExpression, head: InfixExpression):
        '''
        body ↔ head

        "if head then body" or "body if head"
        '''

        self.head = head
        self.body = body
    
    def __str__(self):
        return f"{self.body} ↔ {self.head}"

    def evaluate(self) -> TruthConstant:

        return reverse(self.body.evaluate(), self.head.evaluate())