from interpretation import Interpretation
from program import Program
from clauses import Rule
from abductive_framework import Observation, get_set_of_abducibles, phi_with_abduction, skeptical
from infix.expression import InfixExpression
from phi import phi

from enum import Enum
class Example(Enum):
    # Suppression Task
    _1_AA_PROGRAM = 'e if T;\nl if e and not ab e;\nab e if F'
    _2_AA_ALT_PROGRAM = 'e if T;\nl if e and not ab e;\nl if t and not ab t;\nab e if F;\nab t if F'
    _3_AA_ADD_PROGRAM = 'e if T;\nl if e and not ab e;\nl if o and not ab o;\nab e if not o;ab o if not e'
    _4_DA_PROGRAM = 'e if F;\nl if e and not ab e;\nab e if F'
    _5_DA_ALT_PROGRAM = 'e if F;\nl if e and not ab e;\nl if t and not ab t;\nab e if F;\nab t if F'
    _6_DA_ADD_PROGRAM = 'e if F;\nl if e and not ab e;\nl if o and not ab o;\nab e if not o;\nab o if not e'
    _7_AC_PROGRAM = 'l if e and not ab e;\nab e if F'
    _7_AC_OBSERVATION = 'l'
    _8_AC_ALT_PROGRAM = 'l if e and not ab e;\nl if t and not ab t;\nab e if F;\nab t if F'
    _8_AC_ALT_OBSERVATION = 'l'
    _9_AC_ADD_PROGRAM = 'l if e and not ab e;\nl if o and not ab o;\nab e if not o;\nab o if not e'
    _9_AC_ADD_OBSERVATION = 'l'
    _10_DC_PROGRAM = 'l if e and not ab e;\nab e if F'
    _10_DC_OBSERVATION = 'not l'
    _11_DC_ALT_PROGRAM = 'l if e and not ab e;\nl if t and not ab t;\nab e if F;\nab t if F'
    _11_DC_ALT_OBSERVATION = 'not l'
    _12_DC_ADD_PROGRAM = 'l if e and not ab e;\nl if o and not ab o;\nab e if not o;\nab o if not e'
    _12_DC_ADD_OBSERVATION = 'not l'

    # Disjunctions
    DIS_1_PROGRAM = 'c if F'
    DIS_1_L_DISJUNCTION = 'c'
    DIS_1_R_DISJUNCTION = 'd'
    DIS_1_EXCLUSIVE = False

    DIS_2_PROGRAM = 'c if T'
    DIS_2_L_DISJUNCTION = 'c'
    DIS_2_R_DISJUNCTION = 'd'
    DIS_2_EXCLUSIVE = False

    DIS_3_PROGRAM = 'm if F'
    DIS_3_L_DISJUNCTION = 'a'
    DIS_3_R_DISJUNCTION = 'm'
    DIS_3_EXCLUSIVE = True

    DIS_4_PROGRAM = 'm if T'
    DIS_4_L_DISJUNCTION = 'a'
    DIS_4_R_DISJUNCTION = 'm'
    DIS_4_EXCLUSIVE = True

    DIS_5_PROGRAM = 'h if s and not ab s;\nh if r and not ab r;\nab s if F;\nab r if F'
    DIS_5_L_DISJUNCTION = 's'
    DIS_5_R_DISJUNCTION = 'r'
    DIS_5_EXCLUSIVE = False

    DIS_6_PROGRAM = 'h if s and not ab s;\nh if r and not ab r;\nab s if F;\nab r if F'
    DIS_6_L_DISJUNCTION = 's'
    DIS_6_R_DISJUNCTION = 'r'
    DIS_6_EXCLUSIVE = True

    # Classification of Conditionals
    CLASS_1_PROGRAM = '*c if *a and not ab a;\nab a if F;\na if T'

    CLASS_2_PROGRAM = '*c if *a and not ab a;\nab a if F;\na if F'

    CLASS_3_PROGRAM = '*c if a and not ab a;\nab a if F'
    CLASS_3_OBSERVATION = 'c'

    CLASS_4_PROGRAM = 'c if * a and not ab a;\nab a if F'
    CLASS_4_OBSERVATION = 'not c'

    #Datalog
    DATALOG_1_PROGRAM = '''fly X if bird X and not ab_fly X;\n
                            ab_fly X if kiwi X;\n
                            ab_fly X if penguin X;\n
                            bird tweety if T;\n
                            bird jerry if T'''
    DATALOG_1_OBSERVATION = 'fly jerry'

    DATALOG_2_PROGRAM = '''fly X if bird X and not ab_fly X;\n
                            ab_fly X if F;\n
                            bird tweety if T;\n
                            bird jerry if T'''
    DATALOG_2_OBSERVATION = 'not fly jerry'

    # 
    IA2 = '''b X if c X and not ab_cb X;
            ab_cb X if F;
            c o5 if T;
            b o1 if T;
            b o2 if T;
            a X if b X and not ab_ba X;
            ab_ba o1 if F;
            a o3 if T;
            a o4 if T;
            b X if a X and not ab_ab X;
            ab_ab o3 if F'''






