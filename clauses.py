from abc import ABC, abstractmethod
from truth_constant import TruthConstant
from truth_tables import reverse
from atom import Atom

class Clause(ABC):
    @abstractmethod
    def __str__(self):
        pass
    
    def resolve() -> Atom:
        pass

class Fact(Clause):

    def __init__(self, body: Atom):
        '''
        body ← ⊤ 
        '''

        self.body = body
    
    def __str__(self):
        return f"{self.body.name} ← ⊤"
    
    def resolve(self) -> Atom:
        return reverse(self.body, Atom(TruthConstant.TRUE.value, TruthConstant.TRUE))

class Assumption(Clause):

    def __init__(self, body: Atom):
        '''
        body ← ⊥
        '''
    
        self.body = body

    def __str__(self):
        return f"{self.body.name} ← ⊥"

    def resolve(self) -> Atom:
        return reverse(self.body, Atom(TruthConstant.FALSE.value, TruthConstant.FALSE))

class Rule(Clause):

    def __init__(self, body: Atom, head: Atom):
        '''
        body ← head

        "if head then body" or "body if head"
        '''

        self.head = head
        self.body = body
    
    def __str__(self):
        return f"{self.body.name} ← {self.head.name}"

    def resolve(self) -> Atom:
        return reverse(self.body, self.head)