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

    def __init__(self, body: InfixExpression, head: InfixExpression, non_necessary_antecedent: bool = False, factual_conditional: bool = False):
        '''
        body ← head

        "if head then body" or "body if head"
        '''

        self.head = head
        self.body = body
        self.non_nec = non_necessary_antecedent
        self.factual = factual_conditional
    
    def __repr__(self):
        if self.non_nec == False and self.factual == False:
            return f"{self.body} ← {self.head}"
        elif self.non_nec == True and self.factual == True:
            return f"ⁿⁿ{self.body} ← ᶠ{self.head}"
        elif self.non_nec == True and self.factual == False:
            return f"ⁿⁿ{self.body} ← {self.head}"
        elif self.non_nec == False and self.factual == True:
            return f"{self.body} ← ᶠ{self.head}"

    def __str__(self):
        return self.__repr__()

    def evaluate(self) -> TruthConstant:

        return reverse(self.body.evaluate(), self.head.evaluate())
    
    def get_equv_clause(self):
        return WC_Rule(self.body, self.head, self.non_nec, self.factual)

### Weakly Completed Clauses

class WC_Rule(Clause):

    def __init__(self, body: InfixExpression, head: InfixExpression, non_necessary_antecedent: bool = False, factual_conditional: bool = False):
        '''
        body ↔ head

        "body iff head"
        '''

        self.head = head
        self.body = body
        self.non_nec = non_necessary_antecedent
        self.factual = factual_conditional
    
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        if self.non_nec == False and self.factual == False:
            return f"{self.body} ↔ {self.head}"
        elif self.non_nec == True and self.factual == True:
            return f"ⁿⁿ{self.body} ↔ ᶠ{self.head}"
        elif self.non_nec == True and self.factual == False:
            return f"ⁿⁿ{self.body} ↔ {self.head}"
        elif self.non_nec == False and self.factual == True:
            return f"{self.body} ↔ ᶠ{self.head}"

    def evaluate(self) -> TruthConstant:

        return equivalence(self.body.evaluate(), self.head.evaluate())