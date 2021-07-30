from infix.nodes import *
from truth_tables import *


class Interpreter:
    def visit(self, node, ground_terms:dict):
        '''
        Generic for a method to visit a node and compute.

        Returns the appropriate method for the type of node, i.e. visit_AtomNode
        '''
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name)
        return method(node, ground_terms)

    def visit_AtomNode(self, node, ground_terms):
        return ground_terms[node.atom.string]
    
    def visit_TruthConstNode(self, node, ground_terms):
        return node.truth_constant
    
    def visit_ConjunctionNode(self, node, ground_terms):
        return conjunction(self.visit(node.node_a, ground_terms), self.visit(node.node_b, ground_terms))

    def visit_DisjunctionNode(self, node, ground_terms):
        return disjunction(self.visit(node.node_a, ground_terms), self.visit(node.node_b, ground_terms))

    def visit_NegationNode(self, node, ground_terms):
        return negation(self.visit(node.node, ground_terms))
