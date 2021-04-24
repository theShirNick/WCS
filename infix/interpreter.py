from infix.nodes import *
from truth_tables import *


class Interpreter:
    def visit(self, node):
        '''
        Generic for a method to visit a node and compute.

        Returns the appropriate method for the type of node, i.e. visit_AtomNode
        '''
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name)
        return method(node)

    def visit_AtomNode(self, node):
        return node.atom.ground_value
    
    def visit_ConjunctionNode(self, node):
        return conjunction(self.visit(node.node_a), self.visit(node.node_b))

    def visit_DisjunctionNode(self, node):
        return disjunction(self.visit(node.node_a), self.visit(node.node_b))

    def visit_NegationNode(self, node):
        return negation(self.visit(node.node))
