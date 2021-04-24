from abc import ABC, abstractmethod
from truth_constant import TruthConstant
from truth_tables import reverse
from infix.expression import InfixExpression

class Clause(ABC):
    @abstractmethod
    def __str__(self):
        pass
    
    def evaluate() -> TruthConstant:
        pass

class Fact(Clause):

    def __init__(self, body: InfixExpression):
        '''
        body ← ⊤ 
        '''

        self.body = body
    
    def __str__(self):
        return f"{self.body.expression} ← ⊤"
    
    def evaluate(self) -> TruthConstant:
        return reverse(self.body.evaluate(), TruthConstant.TRUE)

class Assumption(Clause):

    def __init__(self, body: InfixExpression):
        '''
        body ← ⊥
        '''
    
        self.body = body

    def __str__(self):
        return f"{self.body.expression} ← ⊥"

    def evaluate(self) -> TruthConstant:
        return reverse(self.body.evaluate(), TruthConstant.FALSE)

class Rule(Clause):

    def __init__(self, body: InfixExpression, head: InfixExpression):
        '''
        body ← head

        "if head then body" or "body if head"
        '''

        self.head = head
        self.body = body
    
    def __str__(self):
        return f"{self.body.expression} ← {self.head.expression}"

    def evaluate(self) -> TruthConstant:

        return reverse(self.body.evaluate(), self.head.evaluate())