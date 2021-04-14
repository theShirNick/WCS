from atom import *
from truth_constant import * 

def negation(atom1: Atom) -> Atom:
    '''
    Unary NOT (¬) operation for Łukasiewicz three-valued logic

    '''

    atom_result = Atom()
    if atom1.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.FALSE
    elif atom1.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform negation: ¬{atom1.value}")
    return atom_result

def conjunction(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary AND (∧) operation for Łukasiewicz three-valued logic
    '''

    atom_result = Atom()
    if atom1.value == TruthConstant.FALSE or atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.TRUE and atom2.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform conjunction: {atom1.value} ∧ {atom2.value}")
    return atom_result
        
def disjunction(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary OR (∨) operation for Łukasiewicz three-valued logic
    '''

    atom_result = Atom()
    if atom1.value == TruthConstant.TRUE or atom2.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.FALSE and atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform disjunction: {atom1.value} ∨ {atom2.value}")
    return atom_result

def implication(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary material conditional (→) operation for Łukasiewicz three-valued logic

    If atom1 then atom2
    '''

    atom_result = Atom()
    if atom1.value == TruthConstant.FALSE or atom2.value == TruthConstant.TRUE:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN and atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.TRUE and atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform implication: {atom1.value} → {atom2.value}")
    return atom_result

def equivalence(atom1: Atom, atom2: Atom) -> Atom:
    '''
    Binary biconditional (↔) operation for Łukasiewicz three-valued logic

    atom1 iff atom2
    '''

    atom_result = Atom()
    if atom1.value == atom2.value:
        atom_result.value = TruthConstant.TRUE
    elif atom1.value == TruthConstant.UNKNOWN or atom2.value == TruthConstant.UNKNOWN:
        atom_result.value = TruthConstant.UNKNOWN
    elif atom1.value == TruthConstant.FALSE or atom2.value == TruthConstant.FALSE:
        atom_result.value = TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform equivalence: {atom1.value} ↔ {atom2.value}")
    return atom_result