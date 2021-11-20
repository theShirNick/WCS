
import sys
import os
import platform
from collections import deque
from truth_constant import TruthConstant


from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication, QMainWindow, QDialog, QMessageBox
from PySide6.QtCore import QFile, QIODevice, Slot
import PySide6.QtCore as QtCore
from interpretation import Interpretation
from program import Program
from clauses import Rule
from abductive_framework import Observation, generate_explanations, get_set_of_abducibles, phi_with_abduction, skeptical
from infix.expression import InfixExpression
from phi import phi
from examples import Example
from ground import ground, find_vars_and_consts



if __name__ == "__main__":
    app = QApplication(sys.argv)

    ui_file_name = os.path.dirname(__file__) + "/ui/main.ui"
    ui_file = QFile(ui_file_name)
    if not ui_file.open(QIODevice.ReadOnly):
        print(f"Cannot open {ui_file_name}: {ui_file.errorString()}")
        sys.exit(-1)
    loader = QUiLoader()
    window = loader.load(ui_file)
    ui_file.close()
    if not window:
        print(loader.errorString())
        sys.exit(-1)


    ui_file_name = os.path.dirname(__file__) + "/ui/contraction_dialog.ui"
    ui_file = QFile(ui_file_name)
    if not ui_file.open(QIODevice.ReadOnly):
        print(f"Cannot open {ui_file_name}: {ui_file.errorString()}")
        sys.exit(-1)
    loader = QUiLoader()
    contraction_dialog = loader.load(ui_file)
    
    ui_file.close()
    if not contraction_dialog:
        print(loader.errorString())
        sys.exit(-1)


    window.tabWidget.setTabEnabled(0, False)
    window.tabWidget.setTabEnabled(1, False)
    window.tabWidget.setTabEnabled(2, False)
    window.tabWidget.setTabEnabled(3, False)
    window.tabWidget.setTabVisible(4, True)
    window.tabWidget.setCurrentIndex(4)
    window.tabWidget.setTabVisible(5, False)
    # load custom style additions
    QFontDatabase.addApplicationFont(os.path.dirname(__file__) + '/ui/OverpassMono-Regular.ttf')
    QFontDatabase.addApplicationFont(os.path.dirname(__file__) + "/ui/Roboto-Regular.ttf")
    QFontDatabase.addApplicationFont(os.path.dirname(__file__) + "/ui/Symbola.otf")
    

    stylesheet = app.styleSheet()
    with open(os.path.dirname(__file__) + '/ui/custom.css') as file:
        app.setStyleSheet(stylesheet + file.read().format(**os.environ))


    # error tab
    window.tabWidget.setTabVisible(5, False)
    
    
    help_text = '''Example of a valid program input:<br>
<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#ccffcc">fly</font> <font color="aqua">X</font> if <font color="#ccffcc">bird</font> <font color="aqua">X</font> ∧ not <font color="#ccffcc">ab_fly</font> <font color="aqua">X</font>;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#ccffcc">ab_fly</font> <font color="aqua">X</font> if F;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#ccffcc">bird</font> tweety if T;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#ccffcc">bird</font> jerry if T;<br>
<br>
Only datalog programs are supported.<br>
Enter clauses separated by a semicolon.<br>
All clauses must be of the form \"head if body\".<br>
Add an asterisk to the head to make a non-necessary antecedent.<br>
Add an asterisk to the body to make a factual conditional.<br>
Abnormality predicates must begin with \"ab\".<br>
<br>
The 𝒫 tab displays the input program, the ground program, observations, and integrity constraints.<br>
The 𝑤𝑐𝒫 tab shows the groud program its weak completion.<br>
The Φ tab iterates the semantic operator until a fixed point is found.<br>
The 𝒳 tab performs abduction to find explanations beyond the fixed point.<br>
'''
    window.help_textEdit.setHtml(help_text)
    window.tabWidget.setCurrentIndex(4)
    # Important stuff starts here
    global ground_terms
    ground_terms:dict[str, TruthConstant] = dict()
    observations = set()
    clauses = []
    program = Program(clauses)
    wc_program = Program([]) 
    set_of_abducibles = list()
    interpretation_stack = deque()
    integrity_constraints = list()
    explanations = list()
    variables = list()
    consts = list()
    contraction = None
    is_OR = False

    # Program Input
    @Slot()
    def input_program():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)

            program_text = window.input_program_text_edit.toPlainText().replace(':-', '←').replace('-:', '←').replace(' if ', '←')

            if len(program_text) == 0:
                return
            if program_text[-1] == ';':
                program_text = program_text[:-1]
            split_clauses = program_text.split(';')

            for clause in split_clauses:
                try:
                    left_head, right_body = clause.split('←', 1)
                except:
                    if len(str(clause)) == 0:
                        raise Exception(f"Empty clause found. Do you have an extra semicolon somwhere?")
                    raise Exception(f"All clauses must be of the form \"head if body\".<br>This clause is wrong:<br>{clause} ")
                non_nec = False
                factual = False
                if '*' in left_head:
                    non_nec = True
                if '*' in right_body:
                    factual = True
                program.clauses.append(Rule(InfixExpression(left_head, ground_terms), InfixExpression(right_body, ground_terms), non_nec, factual))

            window.input_program_text_edit.setPlaceholderText("")
            P_output()
            window.input_program_text_edit.clear()

        
        except Exception as e:
            show_error(e)
            raise(e)
    # Connect the Program button to the function
    window.input_program_button.clicked.connect(input_program)
    
    # Observation Input
    @Slot()
    def input_observation():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)

            observation_expr = InfixExpression(window.observation_line_edit.text(), ground_terms)
            observations.add(Observation(observation_expr))

            window.observation_line_edit.clear()
           
            P_output()
                
        except Exception as e:
            show_error(e)
            raise(e)
    # Connect the Observation button to the function
    window.input_observation_button.clicked.connect(input_observation)

    # Integrity Constraint Input
    @Slot()
    def input_IC():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)
            # window.tabWidget.setTabVisible(4, False)

            constraint_right_body_expr = InfixExpression(window.constraint_right_body_line_edit.text(), ground_terms)
            constraint_left_head_expr = InfixExpression(window.constraint_left_head_line_edit.text(), ground_terms)
            constraint = Rule(constraint_left_head_expr, constraint_right_body_expr)
            integrity_constraints.append(constraint)

            window.constraint_right_body_line_edit.clear()
            window.constraint_left_head_line_edit.clear()

            P_output()
                
        except Exception as e:
            show_error(e)
            raise(e)
    # Connect the Integrity Constraint button to the function
    window.input_constraint_button.clicked.connect(input_IC)

    # disjunction Input
    @Slot()
    def input_disjunction():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)
            # window.tabWidget.setTabVisible(4, False)

            left_text = window.disjunction_line_edit_left.text()
            right_text = window.disjunction_line_edit_right.text()

            disjunction_of_negated = InfixExpression(f'~{left_text} and ~{right_text}', ground_terms)
            constraint = Rule(InfixExpression("F", ground_terms), disjunction_of_negated)
            integrity_constraints.append(constraint)
            if not is_OR:
                disjunction_for_exclusive = InfixExpression(f'{left_text} and {right_text}', ground_terms)
                constraint = Rule(InfixExpression("F", ground_terms), disjunction_for_exclusive)
                integrity_constraints.append(constraint)

            window.disjunction_line_edit_left.clear()
            window.disjunction_line_edit_right.clear()

            P_output()

        except Exception as e:
            show_error(e)
            raise(e)
    # Connect the Integrity Constraint button to the function
    window.input_disjunction_button.clicked.connect(input_disjunction)

    # Ground, Weakly Complete and construct text
    def wcP():
        window.tabWidget.setTabEnabled(1, True) 
        window.tabWidget.setCurrentIndex(1)
        global program
        global integrity_constraints

        # detect vars and consts in the abductive framework
        find_vars_and_consts(program.clauses, variables, consts)
        find_vars_and_consts(integrity_constraints, variables, consts)

        
        program = Program(ground(program.clauses, variables, consts))
        integrity_constraints = ground(integrity_constraints, variables, consts)

        global set_of_abducibles
        set_of_abducibles = get_set_of_abducibles(ground_terms, program)

        global explanations
        explanations = generate_explanations(set_of_abducibles)

        global wc_program
        wc_program = program.weakly_complete()
        print(ground_terms)
        P_output(True) # call with a wcFlag
    
    # Connect the 'Ground and Weakly Complete' button to the function
    window.to_wc_button.clicked.connect(wcP)

    # Enter Contraction Dialog
    def show_contraction_dialog():
        
        contraction_dialog.exec_()

    # Connect the 'Enter Contraction' button to the function
    window.contraction_button.clicked.connect(show_contraction_dialog)

    # Sumbit Contraction Dialog
    def submit_contraction_dialog():
        truths_input = set()
        if len(contraction_dialog.truths.text()) > 0:
            truths_input = contraction_dialog.truths.text().split(',')
            for i in range(len(truths_input)):
                truths_input[i] = str(InfixExpression(truths_input[i], ground_terms))

        falses_input = set()
        if len(contraction_dialog.falses.text()) > 0:
            falses_input = contraction_dialog.falses.text().split(',')
            for i in range(len(falses_input)):
                falses_input[i] = str(InfixExpression(falses_input[i], ground_terms))
        print(falses_input)

        unknowns_left = set()
        for gt in ground_terms:
            if gt not in falses_input and gt not in truths_input:
                unknowns_left.add(gt)
        global contraction
        contraction = Interpretation(ground_terms, set(truths_input), set(falses_input), unknowns_left)
        print(ground_terms)
        interpretation_stack.append(contraction)
        contraction_dialog.close()

    # Connect the 'Enter Contraction' button to the function
    contraction_dialog.submit_button.clicked.connect(submit_contraction_dialog)



    # Semantic Phi Operator
    def phi_fixed_point():
        window.tabWidget.setTabEnabled(2, True)
        window.tabWidget.setCurrentIndex(2)
        output = ''
        # interpretation_stack.clear()
        window.PhiTextEdit.clear()
        # if len(interpretation_stack) == 0:
        #     interpretation_stack.append(Interpretation(ground_terms, set(), set(), set(ground_terms.keys() )))
        if len(interpretation_stack) == 0:
            interpretation_stack.append(Interpretation(ground_terms, set(), set(), set(ground_terms.keys())))
        
        stop = False
        while stop == False:
            output = output + f"Φ↑{len(interpretation_stack) -1}: {str(interpretation_stack[-1])}<hr>"
            next_phi = phi(wc_program, interpretation_stack[-1])
            if interpretation_stack[-1] == next_phi:
                output = output + f"Fixed point found.<br>"
                stop = True

                integrity_constraint_check = True
                for constraint in integrity_constraints:
                    if constraint.evaluate() != TruthConstant.TRUE:
                        integrity_constraint_check = False
                        break
                if not integrity_constraint_check:
                   output = output + f"Integrity constraint not satisfied. Try abduction.<br>"

                unexplained = set()
                for ob in observations:
                    if ob.ground_term in next_phi.unknowns:
                        unexplained.add(ob)
                if len(unexplained) > 0:
                    obs_str = ''
                    for obs in observations:
                        obs_str = obs_str + str(obs) + ', '
                    if len(unexplained) == 1:
                        output = output + f"Observation {obs_str[:-2]} is unexplained. Abduction may help."
                    else:
                        output = output + f"Observations {obs_str[:-2]} are unexplained. Abduction may help."

            else:     
                interpretation_stack.append(next_phi) 
            window.PhiTextEdit.setHtml(output + "")
    # Connect the 'Find Least Fixed Point' button to the function
    window.to_phi_button.clicked.connect(phi_fixed_point)

    # 𝒳 - explain with abduction
    def abduction():  
        window.tabWidget.setTabEnabled(3, True)
        window.tabWidget.setCurrentIndex(3)
        output = ''
        window.XTextEdit.clear()
        if len(set_of_abducibles) == 0:
            old_s = ''
            old_trues_only = set()
            old_falses_only = set()
            if len(interpretation_stack) > 1:
                old_trues_only = interpretation_stack[-1].trues.copy()
                old_falses_only = interpretation_stack[-1].falses.copy()
                for a in old_trues_only:
                    old_s = old_s + f"{str(a)}, "
                if len(old_trues_only) > 0 and len(old_falses_only) == 0:
                    old_s = old_s[:-2]
                for a in old_falses_only:
                    old_s = old_s + f"¬{str(a)}, "
                if len(old_falses_only) > 0:
                    old_s = old_s[:-2]
                output = output + f"Skeptically, nothing new follows. We already know:<br>{old_s}<br>"
            elif len(interpretation_stack) == 1:
                output = output + f"Empty fixed point, empty set of abducibles. Nothing follows."
            else:
                raise Exception("Interpretation stack empty. Did Phi run correctly?")

        else:
            explanations_interpretations = phi_with_abduction(explanations, program, observations, interpretation_stack[-1], integrity_constraints)
            if len(explanations_interpretations) > 0:
                abduced_interpretations = list()
                for expl, interpr in explanations_interpretations:
                    output = output + f"𝒳 {expl}<br>yields minimal model<br>{interpr}<br><hr>"
                    abduced_interpretations.append(interpr)

                skeptical_result = skeptical(ground_terms, program, abduced_interpretations)
                output = output + skeptical_result

        window.XTextEdit.setHtml(output+ "")
    # Connect the 'Explain with Skeptical Abduction' button to the function
    window.to_x_button.clicked.connect(abduction)
        



    # Exclusive disjunction button
    @Slot()
    def XOR_switch():
        global is_OR
        is_OR = window.exclusive_button.isChecked()
        if is_OR:
            window.exclusive_button.setText("OR")
        else:
            window.exclusive_button.setText("XOR")  
    # Connect the XOR switch to the function  
    window.exclusive_button.clicked.connect(XOR_switch)

    # Call this after input changes -- solve the program
    def wcP_Phi_X():

        wcP()
        phi_fixed_point()
        abduction()
        
    # Clear Program
    @Slot()
    def clear_program():
        window.tabWidget.setTabEnabled(0, False)
        window.tabWidget.setTabEnabled(1, False)
        window.tabWidget.setTabEnabled(2, False)
        window.tabWidget.setTabEnabled(3, False)
        window.tabWidget.setTabVisible(4, True)
        window.tabWidget.setCurrentIndex(4)
        window.tabWidget.setTabVisible(5, False)
        clauses.clear()
        global program
        program = Program([])
        global contraction
        contraction = None
        ground_terms.clear()
        observations.clear()
        consts.clear()
        variables.clear()
        integrity_constraints.clear()
        set_of_abducibles.clear()
        interpretation_stack.clear()
        window.PTextEdit.clear()
        window.wcPTextEdit.clear()
        window.PhiTextEdit.clear()
        window.XTextEdit.clear()
        window.input_program_text_edit.clear()
        global is_OR
        if is_OR:
            window.exclusive_button.nextCheckState()
            is_OR = False
            XOR_switch()
        
    
    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    def P_output(wcFlag = False):
        '''
            Construct and output text containing the program, observations, integrity constraints, and abducibles
        '''
        
        output = ''

        if not wcFlag:
            output = output + "𝓟:<br>"+ str(program)
        elif  wcFlag:
            output = output + "𝑔𝓟:<br>"+ str(program) + '<hr>𝔀𝓬𝓟:<br>' + str(wc_program)
           
        if len (observations) > 0:
            observations_str = "<hr>𝓞:<br>"
            for observation in observations:
                observations_str = observations_str + str(observation) + ';<br>'
            observations_str = observations_str[:-5]
            output =  output + observations_str

        if len (integrity_constraints) > 0:
            IC_string = "<hr>𝓘𝓒:<br>"
            for IC in integrity_constraints:
                IC_string = IC_string + str(IC) + ';<br>'
            IC_string = IC_string[:-5]
            output =  output + IC_string

        if len(set_of_abducibles) > 0:
            abducibles_str = "<hr>𝒜:<br>"
            for abducible in set_of_abducibles: 
                abducibles_str = abducibles_str + str(abducible) + ";<br>"
            abducibles_str = abducibles_str[:-5]
            output=  output + abducibles_str
        
        if not wcFlag:
            window.PTextEdit.clear()
            window.PTextEdit.setHtml(output + "")
        elif wcFlag:
            window.wcPTextEdit.clear()
            window.wcPTextEdit.setHtml(output + "")
    
    def show_error(e):
            window.tabWidget.setTabVisible(5, True)
            window.tabWidget.setCurrentIndex(5)
            window.error_textEdit.clear()
            window.error_textEdit.append(str(e))

    # Run Example
    @Slot()
    def _1_AA():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._1_AA_PROGRAM.value)
        input_program()
    window.action1_AA.triggered.connect(_1_AA)

    @Slot()
    def _2_AA_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._2_AA_ALT_PROGRAM.value)
        input_program()
    window.action2_AA_ALT.triggered.connect(_2_AA_ALT)

    @Slot()
    def _3_AA_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._3_AA_ADD_PROGRAM.value)
        input_program()
    window.action3_AA_ADD.triggered.connect(_3_AA_ADD)

    @Slot()
    def _4_DA():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._4_DA_PROGRAM.value)
        input_program()
    window.action4_DA.triggered.connect(_4_DA)

    @Slot()
    def _5_DA_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._5_DA_ALT_PROGRAM.value)
        input_program()
    window.action5_DA_ALT.triggered.connect(_5_DA_ALT)

    @Slot()
    def _6_DA_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._6_DA_ADD_PROGRAM.value)
        input_program()
    window.action6_DA_ADD.triggered.connect(_6_DA_ADD)

    @Slot()
    def _7_AC():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._7_AC_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._7_AC_OBSERVATION.value)
        input_observation()
    window.action7_AC.triggered.connect(_7_AC)

    @Slot()
    def _8_AC_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._8_AC_ALT_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._8_AC_ALT_OBSERVATION.value)
        input_observation()
    window.action8_AC_ALT.triggered.connect(_8_AC_ALT)

    @Slot()
    def _9_AC_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._9_AC_ADD_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._9_AC_ADD_OBSERVATION.value)
        input_observation()
    window.action9_AC_ADD.triggered.connect(_9_AC_ADD)

    @Slot()
    def _10_DC():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._10_DC_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._10_DC_OBSERVATION.value)
        input_observation()
    window.action10_DC.triggered.connect(_10_DC)

    @Slot()
    def _11_DC_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._11_DC_ALT_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._11_DC_ALT_OBSERVATION.value)
        input_observation()
    window.action11_DC_ALT.triggered.connect(_11_DC_ALT)

    @Slot()
    def _12_DC_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._12_DC_ADD_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._12_DC_ADD_OBSERVATION.value)
        input_observation()
    window.action12_DC_ADD.triggered.connect(_12_DC_ADD)

    @Slot()
    def DIS_1():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_1_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_1_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_1_R_DISJUNCTION.value)
        if not Example.DIS_1_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_1.triggered.connect(DIS_1)

    @Slot()
    def DIS_2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_2_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_2_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_2_R_DISJUNCTION.value)
        if not Example.DIS_2_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_2.triggered.connect(DIS_2)

    @Slot()
    def DIS_3():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_3_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_3_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_3_R_DISJUNCTION.value)
        if not Example.DIS_3_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_3.triggered.connect(DIS_3)

    @Slot()
    def DIS_4():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_4_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_4_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_4_R_DISJUNCTION.value)
        if not Example.DIS_4_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_4.triggered.connect(DIS_4)

    @Slot()
    def DIS_5():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_5_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_5_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_5_R_DISJUNCTION.value)
        if not Example.DIS_5_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_5.triggered.connect(DIS_5)

    @Slot()
    def DIS_6():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DIS_6_PROGRAM.value)
        input_program()
        window.disjunction_line_edit_left.clear()
        window.disjunction_line_edit_right.clear()
        window.disjunction_line_edit_left.setText(Example.DIS_6_L_DISJUNCTION.value)
        window.disjunction_line_edit_right.setText(Example.DIS_6_R_DISJUNCTION.value)
        if not Example.DIS_6_EXCLUSIVE.value:
            window.exclusive_button.nextCheckState()
            global is_OR
            is_OR = True
            XOR_switch()
        input_disjunction()
    window.actionExample_6.triggered.connect(DIS_6)

    @Slot()
    def CLASS_1():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_1_PROGRAM.value)
        input_program()
    window.action1_AA_both.triggered.connect(CLASS_1)

    @Slot()
    def CLASS_2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_2_PROGRAM.value)
        input_program()
    window.action2_DA_both.triggered.connect(CLASS_2)

    @Slot()
    def CLASS_3():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_3_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.CLASS_3_OBSERVATION.value)
        input_observation()
    window.action3_AC_non_necessary.triggered.connect(CLASS_3)

    @Slot()
    def CLASS_4():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_4_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.CLASS_4_OBSERVATION.value)
        input_observation()
    window.action4_DC_factual.triggered.connect(CLASS_4)

    @Slot()
    def DATALOG_1():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DATALOG_1_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.DATALOG_1_OBSERVATION.value)
        input_observation()
    window.action1_Birds_Explicit.triggered.connect(DATALOG_1)

    @Slot()
    def DATALOG_2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DATALOG_2_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.DATALOG_2_OBSERVATION.value)
        input_observation()
    window.action2_Birds_Implicit.triggered.connect(DATALOG_2)

    @Slot()
    def IA2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.IA2.value)
        input_program()
    window.action1_IA2.triggered.connect(IA2)

    @Slot()
    def OA4():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.OA4_PROGRAM.value)
        input_program()
        window.constraint_left_head_line_edit.clear()
        window.constraint_left_head_line_edit.setText(Example.OA4_CONSTRAINT_LEFT.value)
        window.constraint_right_body_line_edit.clear()
        window.constraint_right_body_line_edit.setText(Example.OA4_CONSTRAINT_RIGHT.value)
        input_IC()
    window.action2_OA4.triggered.connect(OA4)

    @Slot()
    def IE4():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.IE4.value)
        input_program()
        window.constraint_left_head_line_edit.clear()
        window.constraint_left_head_line_edit.setText(Example.IE4_CONSTRAINT_LEFT.value)
        window.constraint_right_body_line_edit.clear()
        window.constraint_right_body_line_edit.setText(Example.IE4_CONSTRAINT_RIGHT.value)
        input_IC()
    window.action3_IE4.triggered.connect(IE4)
    
    # run GUI
    window.show()
    sys.exit(app.exec_())