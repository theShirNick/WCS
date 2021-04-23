from atom import *
from truth_constant import * 

def negation(atom1: Atom) -> Atom:
    '''
    Unary NOT (¬) operation for Łukasiewicz three-valued logic

    '''

    atom_result = Atom("test_atom")

    # Find resulting value
    if atom1.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.FALSE
    elif atom1.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform negation: ¬{atom1.value}")
    
    # Name the new atom; use paratheses if needed
    if any(substring in atom1.name for substring in ["∧", "∨", "→","←", "↔" ]):
        atom_result.name = f"¬({atom1.name})"
    else:
        atom_result.name = f"¬{atom1.name}"
    return atom_result

def conjunction(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary AND (∧) operation for Łukasiewicz three-valued logic
    '''

    atom_result = Atom("test_atom")
    if atom1.value == TruthConstant.FALSE or atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.TRUE and atom2.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform conjunction: {atom1.value} ∧ {atom2.value}")

    # Name the new atom; use TODO paratheses if needed
    if any(substring in atom1.name for substring in ["∨", "←", "→", "↔" ]) and any(substring in atom2.name for substring in ["∨", "→", "↔" ]):
        atom_result.name = f"({atom1.name})∧({atom2.name})"
    elif any(substring in atom1.name for substring in ["∨", "→", "↔" ]):
        atom_result.name = f"({atom1.name})∧{atom2.name}"
    elif any(substring in atom2.name for substring in ["∨", "→", "↔" ]):
        atom_result.name = f"{atom1.name}∧({atom2.name})"
    else:
        atom_result.name = f"{atom1.name}∧{atom2.name}"
    return atom_result
        
def disjunction(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary OR (∨) operation for Łukasiewicz three-valued logic
    '''

    atom_result = Atom("test_atom")

    # Find resulting value
    if atom1.value == TruthConstant.TRUE or atom2.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.FALSE and atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform disjunction: {atom1.value} ∨ {atom2.value}")

    # Name the new atom; use paratheses if needed
    if any(substring in atom1.name for substring in ["∧", "←", "→", "↔" ]) and any(substring in atom2.name for substring in ["∧", "→", "↔" ]):
        atom_result.name = f"({atom1.name}∨{atom2.name})"
    else:
        atom_result.name = f"{atom1.name}∨{atom2.name}"
    return atom_result

def implication(head: Atom, body: Atom) -> Atom:
    '''
    Binary material conditional (→) operation for Łukasiewicz three-valued logic

    head → body

    "If head then body"
    '''

    atom_result = Atom("test_atom")

    # Find resulting value
    if head.value == TruthConstant.FALSE or body.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    elif head.value == TruthConstant.UNKNOWN and body.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.TRUE
    elif head.value == TruthConstant.UNKNOWN or body.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif head.value == TruthConstant.TRUE and body.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform implication: {head.value} → {body.value}")

    # Name the new atom; TODO use paratheses if needed
    atom_result.name=f"{head.name}→{body.name}"
    return atom_result

def reverse(body: Atom, head: Atom) -> Atom:
    '''
    Reverse of implication (←) operation for Łukasiewicz three-valued logic

    body ← head

    "body if head"
    '''

    atom_result = Atom("test_atom")

    # Find resulting value
    if head.value == TruthConstant.FALSE or body.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    elif head.value == TruthConstant.UNKNOWN and body.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.TRUE
    elif head.value == TruthConstant.UNKNOWN or body.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif head.value == TruthConstant.TRUE and body.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform  reverse implication: {body.value} ← {head.value}")

    # Name the new atom; TODO use paratheses if needed
    atom_result.name=f"{body.name}←{head.name}"
    return atom_result

def equivalence(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary biconditional (↔) operation for Łukasiewicz three-valued logic

    atom1 iff atom2
    '''

    atom_result = Atom("test_atom")

    # Find resulting value
    if atom1.value == atom2.value:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.FALSE or atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform equivalence: {atom1.value} ↔ {atom2.value}")

    # Name the new atom; TODO use paratheses if needed
    atom_result.name=f"{atom1.name}↔{atom2.name}"
    return atom_result