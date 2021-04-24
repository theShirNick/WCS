from dataclasses import dataclass
from atom import Atom

@dataclass
class AtomNode:
    atom: Atom

    def __repr__(self) -> str:
        return f"{self.atom}"

@dataclass
class ConjunctionNode:
    node_a: any
    node_b: any

    def __repr__(self) -> str:
        return f"({self.node_a} ∧ {self.node_b})"

@dataclass
class DisjunctionNode:
    node_a: any
    node_b: any

    def __repr__(self) -> str:
        return f"({self.node_a} ∨ {self.node_b})"

@dataclass
class NegationNode:
    node: any

    def __repr__(self) -> str:
        return f"(¬{self.node})"