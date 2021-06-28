from abc import ABC, abstractmethod
from logging import fatal
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

    def __init__(self, left_head: InfixExpression, right_body: InfixExpression, non_necessary_antecedent: bool = False, factual_conditional: bool = False):
        '''
        head ← body

        "if body then head" or "head if body"
        '''

        self.right_body = right_body
        self.left_head = left_head
        self.non_nec = non_necessary_antecedent
        self.factual = factual_conditional
    
    def __repr__(self):
        left = str(self.left_head)
        if self.non_nec:
            left = f"**{self.left_head}**"
        right = str(self.right_body)
        if self.factual:
            right = self.right_body.bold_abnormalities()
        # if self.non_nec == False and self.factual == False:
        #     return f"{self.left_head} ← {self.right_body}"
        # elif self.non_nec == True and self.factual == True:
        #     return f"ⁿⁿ{self.left_head} ← ᶠ{self.right_body}"
        # elif self.non_nec == True and self.factual == False:
        #     return f"ⁿⁿ{self.left_head} ← {self.right_body}"
        # elif self.non_nec == False and self.factual == True:
        #     return f"{self.left_head} ← ᶠ{self.right_body}"

        return f"{left} ← {right}"

    def __str__(self):
        return self.__repr__()
    
    def __eq__(self, o: object) -> bool:
        if self.left_head == o.left_head and self.right_body == o.right_body and self.non_nec == o.non_nec and self.factual == o.factual:
            return True
        return False
    
    def __hash__(self) -> int:
        return self.left_head.__hash__() + self.right_body.__hash__() + self.non_nec.__hash__() + (3*self.factual.__hash__())

    def evaluate(self) -> TruthConstant:

        return reverse(self.left_head.evaluate(), self.right_body.evaluate())
    
    def get_equv_clause(self):
        return WC_Rule(self.left_head, self.right_body, self.non_nec, self.factual)

### Weakly Completed Clauses

class WC_Rule(Clause):

    def __init__(self, left_head: InfixExpression, right_body: InfixExpression):
        '''
        head ↔ body

        "head iff body"
        '''

        self.right_body = right_body
        self.left_head = left_head
    
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
            return f"{self.left_head} ↔ {self.right_body}"

    def evaluate(self) -> TruthConstant:

        return equivalence(self.left_head.evaluate(), self.right_body.evaluate())