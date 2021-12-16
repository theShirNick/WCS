from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(wc_program:Program, interpretation:Interpretation) -> Interpretation:
# TODO: DRY condence the three chunks of code
 
    new_interpretation = interpretation.clone()

    # move true atoms to either falses or unknowns
    from_T_to_U = set()
    from_T_to_F = set()
    for true_term in new_interpretation.trues:
        needs_correction = True
        all_F_flag = None
        for clause in wc_program.clauses:
            term = str(clause.left_head)
            if term == true_term:
                if clause.right_body.evaluate() == TruthConstant.TRUE:
                    needs_correction = False
                    break
                elif clause.right_body.evaluate() == TruthConstant.FALSE and all_F_flag == None:
                    all_F_flag = True # there were no Ts of Us
                elif  clause.right_body.evaluate() == TruthConstant.UNKNOWN:
                    all_F_flag = False
        if needs_correction:
            if all_F_flag == True:
                from_T_to_F.add(true_term)
            else: # False or None
                from_T_to_U.add(true_term)

            
    # move false atoms to either trues or unknowns
    from_F_to_U = set()
    from_F_to_T = set()
    for false_term in new_interpretation.falses:
        T_flag = False
        all_F_flag = None
        for clause in wc_program.clauses:
            term = str(clause.left_head)
            if term == false_term:
                if clause.right_body.evaluate() == TruthConstant.FALSE and all_F_flag == None:
                    all_F_flag = True
                elif clause.right_body.evaluate() == TruthConstant.TRUE:
                    T_flag = True
                    all_F_flag = False
                    break
                elif clause.right_body.evaluate() == TruthConstant.UNKNOWN:
                    all_F_flag = False
                    
        if T_flag == True:
            from_F_to_T.add(false_term)
        elif all_F_flag !=True:
            from_F_to_U.add(false_term)

    # move unknown atoms to either falses or trues 
    from_U_to_T = set()
    from_U_to_F = set()
    for unknown_term in new_interpretation.unknowns:
        T_flag = False
        all_F_flag = None
        for clause in wc_program.clauses:
            term = str(clause.left_head)
            if term == unknown_term:
                if clause.right_body.evaluate() == TruthConstant.FALSE and all_F_flag == None:
                    all_F_flag = True
                elif clause.right_body.evaluate() == TruthConstant.TRUE:
                    T_flag = True
                    all_F_flag = False
                    break
                elif clause.right_body.evaluate() == TruthConstant.UNKNOWN:
                    all_F_flag = False
                    
        if T_flag == True:
            from_U_to_T.add(unknown_term)
        elif all_F_flag ==True:
            from_U_to_F.add(unknown_term)

    # apply movement (one phi iteration) 
    for term in from_T_to_U:
        interpretation.ground_terms[term] = TruthConstant.UNKNOWN
        new_interpretation.trues.remove(term)
        new_interpretation.unknowns.add(term)
    for term in from_T_to_F:
        interpretation.ground_terms[term] = TruthConstant.FALSE
        new_interpretation.trues.remove(term)
        new_interpretation.falses.add(term)
    for term in from_F_to_U:
        interpretation.ground_terms[term] = TruthConstant.UNKNOWN
        new_interpretation.falses.remove(term)
        new_interpretation.unknowns.add(term)
    for term in from_F_to_T:
        interpretation.ground_terms[term] = TruthConstant.TRUE
        new_interpretation.falses.remove(term)
        new_interpretation.trues.add(term)
    for term in from_U_to_T:
        interpretation.ground_terms[term] = TruthConstant.TRUE
        new_interpretation.unknowns.remove(term)
        new_interpretation.trues.add(term)
    for term in from_U_to_F:
        interpretation.ground_terms[term] = TruthConstant.FALSE
        new_interpretation.unknowns.remove(term)
        new_interpretation.falses.add(term)

    return new_interpretation