from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(wc_program:Program, interpretation:Interpretation) -> Interpretation:

    new_interpretation = interpretation.clone()

    # sanity check for already true atoms
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

            
    # sanity check for already false atoms
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

    # fix interpretation that failed the sanity check
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


    # # discover immediate consequences
    # candidates_for_false = dict()
    # to_true = set()

    # for clause in wc_program.clauses:
    #     consequence_term = str(clause.left_head)
    #     if consequence_term not in interpretation.ground_terms:
    #         raise Exception("Head of a clause is not a ground term.")
        

    #     if clause.right_body.evaluate() == TruthConstant.TRUE:
    #         to_true.add(consequence_term)

    #         if consequence_term in candidates_for_false:
    #             candidates_for_false[consequence_term] = False

    #     elif clause.right_body.evaluate() == TruthConstant.FALSE:
    #         if consequence_term not in candidates_for_false:
    #             candidates_for_false[consequence_term] = True
    #     elif clause.right_body.evaluate() == TruthConstant.UNKNOWN:
    #         if consequence_term in candidates_for_false:
    #             candidates_for_false[consequence_term] = False



    # for term in to_true:
    #     interpretation.ground_terms[term] = TruthConstant.TRUE
    #     if term in new_interpretation.unknowns:
    #         new_interpretation.unknowns.remove(term)
    #         new_interpretation.trues.add(term)
    #     if term in new_interpretation.falses:
    #         new_interpretation.falses.remove(term)
    #         new_interpretation.trues.add(term)
        
    # for term in candidates_for_false:
    #     if candidates_for_false[term] == True:
    #         interpretation.ground_terms[term] = TruthConstant.FALSE
    #         if term in new_interpretation.unknowns:
    #             new_interpretation.unknowns.remove(term)
    #             new_interpretation.falses.add(term)


    return new_interpretation