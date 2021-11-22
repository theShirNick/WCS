from truth_constant import * 

def negation(truth_val: TruthConstant) -> TruthConstant:
    '''
    Unary NOT (¬) operation for Łukasiewicz three-valued logic
    '''

    # Find resulting value
    if truth_val == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif truth_val == TruthConstant.TRUE:
        return TruthConstant.FALSE
    elif truth_val == TruthConstant.FALSE:
        return TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform negation: ¬{truth_val}")

def context(truth_val: TruthConstant) -> TruthConstant:
    '''
    Unary ctxt operation for Łukasiewicz three-valued logic
    '''

    # Find resulting value
    if truth_val == TruthConstant.UNKNOWN:
        return TruthConstant.FALSE
    elif truth_val == TruthConstant.TRUE:
        return TruthConstant.TRUE
    elif truth_val == TruthConstant.FALSE:
        return TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform negation: ctxt {truth_val}")
    

def conjunction(truth_val1: TruthConstant, truth_val2: TruthConstant) -> TruthConstant:
    '''
    Binary AND (∧) operation for Łukasiewicz three-valued logic
    '''

    if truth_val1 == TruthConstant.FALSE or truth_val2 == TruthConstant.FALSE:
        return TruthConstant.FALSE
    elif truth_val1 == TruthConstant.UNKNOWN or truth_val2 == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif truth_val1 == TruthConstant.TRUE and truth_val2 == TruthConstant.TRUE:
        return TruthConstant.TRUE
    else:
        raise Exception(f"Could not perform conjunction: {truth_val1} ∧ {truth_val2}")

        
def disjunction(truth_val1: TruthConstant, truth_val2: TruthConstant) -> TruthConstant:
    '''
    Binary OR (∨) operation for Łukasiewicz three-valued logic
    '''

    # Find resulting value
    if truth_val1 == TruthConstant.TRUE or truth_val2 == TruthConstant.TRUE:
        return TruthConstant.TRUE
    elif truth_val1 == TruthConstant.UNKNOWN or truth_val2 == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif truth_val1 == TruthConstant.FALSE and truth_val2 == TruthConstant.FALSE:
        return TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform disjunction: {truth_val1} ∨ {truth_val2}")

def implication(left_head_value: TruthConstant, right_body_value: TruthConstant) -> TruthConstant:
    '''
    (UNUSED) Binary material conditional (→) operation for Łukasiewicz three-valued logic

    head → body

    "If head then body"
    '''

    # Find resulting value
    if left_head_value == TruthConstant.FALSE or right_body_value == TruthConstant.TRUE:
        return TruthConstant.TRUE
    elif left_head_value == TruthConstant.UNKNOWN and right_body_value == TruthConstant.UNKNOWN:
        return TruthConstant.TRUE
    elif left_head_value == TruthConstant.UNKNOWN or right_body_value == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif left_head_value == TruthConstant.TRUE and right_body_value == TruthConstant.FALSE:
        return TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform implication: {left_head_value} → {right_body_value}")

def reverse(left_head_value: TruthConstant, right_body_value: TruthConstant) -> TruthConstant:
    '''
    Reverse of implication (←) operation for Łukasiewicz three-valued logic

    head ← body

    "head if body"
    '''

    # Find resulting value
    if right_body_value == TruthConstant.FALSE or left_head_value == TruthConstant.TRUE:
        return TruthConstant.TRUE
    elif right_body_value == TruthConstant.UNKNOWN and left_head_value == TruthConstant.UNKNOWN:
        return TruthConstant.TRUE
    elif right_body_value == TruthConstant.UNKNOWN or left_head_value == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif right_body_value == TruthConstant.TRUE and left_head_value == TruthConstant.FALSE:
        return TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform  reverse implication: {left_head_value} ← {right_body_value}")

def equivalence(truth_val1: TruthConstant, truth_val2: TruthConstant) -> TruthConstant:
    '''
    Binary biconditional (↔) operation for Łukasiewicz three-valued logic

    truth_val1 iff truth_val2
    '''

    # Find resulting value
    if truth_val1 == truth_val2:
        return TruthConstant.TRUE
    elif truth_val1 == TruthConstant.UNKNOWN or truth_val2 == TruthConstant.UNKNOWN:
        return TruthConstant.UNKNOWN
    elif truth_val1 == TruthConstant.FALSE or truth_val2 == TruthConstant.FALSE:
        return TruthConstant.FALSE
    else:
        raise Exception(f"Could not perform equivalence: {truth_val1} ↔ {truth_val2}")

