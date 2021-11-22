from program import Program
from interpretation import Interpretation
from truth_constant import TruthConstant

def phi(wc_program:Program, interpretation:Interpretation) -> Interpretation:

    new_interpretation = interpretation.clone()

    # sanity check for already true atoms
    untrue = set()
    for true_term in new_interpretation.trues:
        sanity_pass = False
        for clause in wc_program.clauses:
            term = str(clause.left_head)
            if term == true_term:
                if clause.right_body.evaluate() == TruthConstant.TRUE:
                    sanity_pass = True
                    break
        if sanity_pass == False:
            untrue.add(true_term)

            
    # sanity check for already false atoms
    unfalse = set()
    for false_term in new_interpretation.falses:
        sanity_pass = None
        for clause in wc_program.clauses:
            term = str(clause.left_head)
            if term == false_term:
                if clause.right_body.evaluate() == TruthConstant.FALSE:
                    sanity_pass = True
                else:
                    sanity_pass = False
                    break
        if sanity_pass != True:
            unfalse.add(false_term)

    # fix interpretation that failed the sanity check
    for term in untrue:
        interpretation.ground_terms[term] = TruthConstant.UNKNOWN
        new_interpretation.trues.remove(term)
        new_interpretation.unknowns.add(term)

    for term in unfalse:
        interpretation.ground_terms[term] = TruthConstant.UNKNOWN
        new_interpretation.falses.remove(term)
        new_interpretation.unknowns.add(term)


    # get immediate consequences
    candidates_for_false = dict()
    to_true = set()

    for clause in wc_program.clauses:
        consequence_term = str(clause.left_head)
        if consequence_term not in interpretation.ground_terms:
            raise Exception("Head of a clause is not a ground term.")
        

        if clause.right_body.evaluate() == TruthConstant.TRUE:
            to_true.add(consequence_term)

            if consequence_term in candidates_for_false:
                candidates_for_false[consequence_term] = False

        elif clause.right_body.evaluate() == TruthConstant.FALSE:
            if consequence_term not in candidates_for_false:
                candidates_for_false[consequence_term] = True
        elif clause.right_body.evaluate() == TruthConstant.UNKNOWN:
            if consequence_term in candidates_for_false:
                candidates_for_false[consequence_term] = False



    for term in to_true:
        interpretation.ground_terms[term] = TruthConstant.TRUE
        if term in new_interpretation.unknowns:
            new_interpretation.unknowns.remove(term)
            new_interpretation.trues.add(term)
        if term in new_interpretation.falses:
            new_interpretation.falses.remove(term)
            new_interpretation.trues.add(term)
        
    for term in candidates_for_false:
        if candidates_for_false[term] == True:
            interpretation.ground_terms[term] = TruthConstant.FALSE
            if term in new_interpretation.unknowns:
                new_interpretation.unknowns.remove(term)
                new_interpretation.falses.add(term)


      
   
    return new_interpretation