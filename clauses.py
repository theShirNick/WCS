from abc import ABC, abstractmethod
from atom import Atom

class Clause(ABC):
    @abstractmethod
    def __str__(self):
        pass


class Fact(Clause):

    def __init__(self, body: Atom):
        '''
        body ← T 
        '''

        self.body = body
    
    def __str__(self):
        return f"{self.body.name} ← T"

class Assumption(Clause):

    def __init__(self, body: Atom):
        '''
        body ← ⊥
        '''
    
        self.body = body

    def __str__(self):
        return f"{self.body.name} ← ⊥"

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