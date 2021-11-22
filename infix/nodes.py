from dataclasses import dataclass
from truth_constant import TruthConstant
from atom import Atom

@dataclass
class AtomNode:
    atom: Atom

    def __repr__(self) -> str:
        return self.atom.string        

@dataclass
class TruthConstNode:
    truth_constant: TruthConstant

    def __repr__(self) -> str:
        return f"{self.truth_constant.value}"

@dataclass
class ConjunctionNode:
    node_a: any
    node_b: any

    def __repr__(self) -> str:
        return f"{self.node_a} ∧ {self.node_b}"

@dataclass
class DisjunctionNode:
    node_a: any
    node_b: any

    def __repr__(self) -> str:
        str = ''
        if type(self.node_a) in [AtomNode, TruthConstNode]:
            str = f"{self.node_a} ∨ "
        else:
            str = f"({self.node_a}) ∨ "

        if type(self.node_b) in [AtomNode, TruthConstNode]:
            str = str + f"{self.node_b}"
        else:
            str = str + f"({self.node_b})"
        return str

@dataclass
class NegationNode:
    node: any

    def __repr__(self) -> str:
        if type(self.node) in [AtomNode, TruthConstNode]:
            return f"¬{self.node}"
        else:
            return f"¬({self.node})"

@dataclass
class ContextNode:
    node: any

    def __repr__(self) -> str:
        if type(self.node) in [AtomNode, TruthConstNode]:
            return f"ctxt {self.node}"
        else:
            return f"ctxt ({self.node})"