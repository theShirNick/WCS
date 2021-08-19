import sys
import os
from collections import deque
from truth_constant import TruthConstant

from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase, QFont
from PySide6.QtWidgets import QApplication, QMainWindow, QDialog
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

from interpretation import Interpretation
from program import Program
from clauses import Rule
from abductive_framework import Observation, generate_explanations, get_set_of_abducibles, phi_with_abduction, skeptical
from infix.expression import InfixExpression
from phi import phi
from examples import Example

from pathlib import Path


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

    # setup stylesheet
    # QFontDatabase.addApplicationFont('ui/OverpassMono-Regular.ttf')
    QFontDatabase.addApplicationFont(os.path.dirname(__file__) + '/ui/OverpassMono-Regular.ttf')
    

    font_id_reg = QFontDatabase.addApplicationFont(os.path.dirname(__file__) + "/ui/Roboto-Regular.ttf")
    apply_stylesheet(app, theme='dark_lightgreen.xml')

    # load custom style additions
    stylesheet = app.styleSheet()
    with open(os.path.dirname(__file__) + '/ui/custom.css') as file:
        app.setStyleSheet(stylesheet + file.read().format(**os.environ))

    
    # force style
    window.statusbar.hide()
    window.PTextEdit.setFont(QFont("Overpass Mono", 12))
    window.PTextEdit.setProperty('class', 'output_text_edit')
    window.Ptab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(1)

    window.wcPTextEdit.setFont(QFont("Overpass Mono", 12))
    window.wcPTextEdit.setProperty('class', 'output_text_edit')
    window.wcPtab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(2)

    window.PhiTextEdit.setFont(QFont("Overpass Mono", 12))
    window.PhiTextEdit.setProperty('class', 'output_text_edit')
    window.Phitab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(3)

    window.XTextEdit.setFont(QFont("Overpass Mono", 12))
    window.XTextEdit.setProperty('class', 'output_text_edit')
    window.Xtab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(4)

    window.help_textEdit.setFont(QFont("Overpass Mono", 12))
    window.help_textEdit.setProperty('class', 'output_text_edit')
    window.help_tab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(5)

    window.error_textEdit.setFont(QFont("Overpass Mono", 12))
    window.error_textEdit.setProperty('class', 'output_text_edit')
    window.error_tab.setFont('Overpass Mono')
    window.tabWidget.setCurrentIndex(4)


    # error tab
    window.tabWidget.setTabVisible(5, False)
    


    window.observation_line_edit.setProperty('class', 'mono_font')
    window.constraint_left_head_line_edit.setProperty('class', 'mono_font')
    window.constraint_right_body_line_edit.setProperty('class', 'mono_font')
    window.input_program_text_edit.setProperty('class', 'background_active_input')
    window.clear_program_button.setProperty('class', 'danger')
    
    # placeholder_text = "Enter clauses separated by a semicolon (;)\nAll clauses must be of the form \"head if body\"\nAn asterisk (*) in the head makes it a clause with a non-necessary antecedent.\nAn asterisk (*) in the body to makes it a factual conditional."
    # window.input_program_text_edit.setPlaceholderText(placeholder_text)
    help_text = '''Example of a valid program input:

    fly X if bird X ∧ not ab_fly X;
    ab_fly X if F;
    bird tweety if T;
    bird jerry if ⊤;

Only datalog programs are supported.
Enter clauses separated by a semicolon.
All clauses must be of the form \"head if body\".
Add an asterisk to the head to make a non-necessary antecedent.
Add an asterisk to the head to make a factual conditional.
Abnormality predicates must begin with \"ab\".
'''
    window.help_textEdit.setText(help_text)
    # Important stuff starts here
    ground_terms:dict[str, TruthConstant] = dict()
    observations = set()
    clauses = []
    program = Program(clauses)
    ground_program:Program = Program([])
    wc_program = Program([]) 
    set_of_abducibles = list()
    interpretation_stack = deque()
    integrity_constraints = set()
    explanations = list()
    
    is_OR = False

    
    # Program Input
    @Slot()
    def input_program():
        try:
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabVisible(4, False)

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
                    raise Exception(f"All clauses must be of the form \"head if body\".\nThis clause is wrong:\n{clause} ")
                non_nec = False
                factual = False
                if '*' in left_head:
                    non_nec = True
                if '*' in right_body:
                    factual = True
                program.clauses.append(Rule(InfixExpression(left_head, ground_terms), InfixExpression(right_body, ground_terms), non_nec, factual))
            global ground_program
            ground_program = program.ground()
            window.input_program_text_edit.setPlaceholderText("")
            window.input_program_text_edit.clear()

            window.PTextEdit.clear()
            window.PTextEdit.append("𝓟:\n" + str(program))
            if ground_program != program:
                window.PTextEdit.append("---\n𝑔𝓟:\n" + str(ground_program))
            window.PTextEdit.append(get_after_P())
            window.PTextEdit.setMarkdown(window.PTextEdit.toMarkdown())
            # Remake the other tabs
            wcP_Phi_X()
        
        except Exception as e:
            show_error(e)
            raise(e)

    # Connect the Program button to the function
    window.input_program_button.clicked.connect(input_program)
    
    # Observation Input
    @Slot()
    def input_observation():
        try:
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabVisible(4, False)

            observation_expr = InfixExpression(window.observation_line_edit.text(), ground_terms)
            observations.add(Observation(observation_expr))

            window.observation_line_edit.clear()

            window.PTextEdit.clear()
            window.PTextEdit.append("𝓟:\n" + str(program))
            if ground_program != program:
                window.PTextEdit.append("---\n𝑔𝓟:\n" + str(ground_program))
            window.PTextEdit.append(get_after_P())
            window.PTextEdit.setMarkdown(window.PTextEdit.toMarkdown())
            # Remake the other tabs 
            wcP_Phi_X()
                
        except Exception as e:
            show_error(e)
            raise(e)

    # Connect the Observation button to the function
    window.input_observation_button.clicked.connect(input_observation)

    # Integrity Constraint Input
    @Slot()
    def input_IC():
        try:
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabVisible(4, False)

            constraint_right_body_expr = InfixExpression(window.constraint_right_body_line_edit.text(), ground_terms)
            constraint_left_head_expr = InfixExpression(window.constraint_left_head_line_edit.text(), ground_terms)
            constraint = Rule(constraint_left_head_expr, constraint_right_body_expr)
            integrity_constraints.add(constraint)

            window.constraint_right_body_line_edit.clear()
            window.constraint_left_head_line_edit.clear()

            window.PTextEdit.clear()
            window.PTextEdit.append("𝓟:\n" + str(program))
            if ground_program != program:
                window.PTextEdit.append("---\n𝑔𝓟:\n" + str(ground_program))
            window.PTextEdit.append(get_after_P())
            window.PTextEdit.setMarkdown(window.PTextEdit.toMarkdown())
            # Remake the other tabs
            wcP_Phi_X()
                
        except Exception as e:
            show_error(e)
            raise(e)

    # Connect the Integrity Constraint button to the function
    window.input_constraint_button.clicked.connect(input_IC)

    # disjunction Input
    @Slot()
    def input_disjunction():
        try:
                
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabVisible(4, False)

            left_text = window.disjunction_line_edit_left.text()
            right_text = window.disjunction_line_edit_right.text()

            disjunction_of_negated = InfixExpression(f'~{left_text} and ~{right_text}', ground_terms)
            constraint = Rule(InfixExpression("F", ground_terms), disjunction_of_negated)
            integrity_constraints.add(constraint)
            if not is_OR:
                disjunction_for_exclusive = InfixExpression(f'{left_text} and {right_text}', ground_terms)
                constraint = Rule(InfixExpression("F", ground_terms), disjunction_for_exclusive)
                integrity_constraints.add(constraint)

            window.disjunction_line_edit_left.clear()
            window.disjunction_line_edit_right.clear()

            window.PTextEdit.clear()
            window.PTextEdit.append("𝓟:\n" + str(program))
            if ground_program != program:
                window.PTextEdit.append("---\n𝑔𝓟:\n" + str(ground_program))
            window.PTextEdit.append(get_after_P())
            window.PTextEdit.setMarkdown(window.PTextEdit.toMarkdown())
            # Remake the other tabs
            wcP_Phi_X()
                    
        except Exception as e:
            show_error(e)
            raise(e)

    # Connect the Integrity Constraint button to the function
    window.input_disjunction_button.clicked.connect(input_disjunction)

    # Weakly Complete and construct text
    def wcP(): 
        global wc_program
        wc_program = ground_program.weakly_complete()

        window.wcPTextEdit.clear()
        window.wcPTextEdit.append("𝔀𝓬𝓟:\n" + str(wc_program))
        window.wcPTextEdit.append(get_after_P())
        window.wcPTextEdit.setMarkdown(window.wcPTextEdit.toMarkdown())
    # Semantic Phi Operator
    def phi_fixed_point():
        interpretation_stack.clear()
        window.PhiTextEdit.clear()
        if len(interpretation_stack) == 0:
            interpretation_stack.append(Interpretation(ground_terms, set(), set(), set(ground_terms.keys() )))
        
        stop = False
        while stop == False:
            window.PhiTextEdit.append(f"Φ↑{len(interpretation_stack) -1}: {str(interpretation_stack[-1])}")
            next_phi = phi(wc_program, interpretation_stack[-1])
            if interpretation_stack[-1] == next_phi:
                window.PhiTextEdit.append(f"Fixed point found.\n")
                stop = True

                integrity_constraint_check = True
                for constraint in integrity_constraints:
                    if constraint.evaluate() != TruthConstant.TRUE:
                        integrity_constraint_check = False
                        break
                if not integrity_constraint_check:
                    window.PhiTextEdit.append(f"Integrity constraint not satisfied. Try abduction.\n")

                unexplained = set()
                for ob in observations:
                    if ob.ground_term in next_phi.unknowns:
                        unexplained.add(ob)
                if len(unexplained) > 0:
                    obs_str = ''
                    for obs in observations:
                        obs_str = obs_str + str(obs) + ', '
                    if len(unexplained) == 1:
                        window.PhiTextEdit.append(f"Observation {obs_str[:-2]} is unexplained. Abduction may help.")
                    else:
                        window.PhiTextEdit.append(f"Observations {obs_str[:-2]} are unexplained. Abduction may help.")

            else:     
                interpretation_stack.append(next_phi) 
            window.PhiTextEdit.setMarkdown(window.PhiTextEdit.toMarkdown())
        
    # 𝒳 - explain with abduction
    def abduction():  
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
                window.XTextEdit.append(f"Skeptically, nothing new follows. We already know:\n{old_s}")
            elif len(interpretation_stack) == 1:
                window.XTextEdit.append(f"Empty fixed point, emplty set of abducibles. Nothing follows.")
            else:
                window.XTextEdit.append("ERROR: Interpretation stack empty. Did Phi run correctly?")

        else:
            explanations_interpretations = phi_with_abduction(explanations, ground_program, observations, interpretation_stack[-1], integrity_constraints)
            if len(explanations_interpretations) > 0:
                abduced_interpretations = list()
                for expl, interpr in explanations_interpretations:
                    window.XTextEdit.append(f"𝒳 {expl}\nyields minimal model\n{interpr}\n---")
                    abduced_interpretations.append(interpr)

                skeptical_result = skeptical(ground_terms, ground_program, abduced_interpretations)
                window.XTextEdit.append(skeptical_result)

        window.XTextEdit.setMarkdown(window.XTextEdit.toMarkdown())


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

    # Call this after input changes
    def wcP_Phi_X():
        wcP()
        phi_fixed_point()
        abduction()
        
    # Clear Program
    @Slot()
    def clear_program():
        window.tabWidget.setTabVisible(4, True)
        window.tabWidget.setCurrentIndex(4)
        window.tabWidget.setTabVisible(5, False)
        clauses.clear()
        global ground_program
        ground_program = Program([])
        ground_terms.clear()
        observations.clear()
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

    def get_after_P():
        '''
            Construct and output text containing observations, integrity constraints, and abducibles
        '''

        after_P_str = ""
        if len (observations) > 0:
            observations_str = "---\n\n𝓞:\n"
            for observation in observations:
                observations_str = observations_str + str(observation) + '\n'
            after_P_str =  after_P_str + observations_str + '\n'

        if len (integrity_constraints) > 0:
            IC_string = "---\n𝓘𝓒:\n"
            for IC in integrity_constraints:
                IC_string = IC_string + str(IC) + '\n'
            after_P_str =  after_P_str + IC_string + '\n'

        global set_of_abducibles
        set_of_abducibles = get_set_of_abducibles(ground_terms, ground_program)
        global explanations
        explanations = generate_explanations(set_of_abducibles)
        if len(set_of_abducibles) > 0:
            abducibles_str = "---\n𝒜:\n"
            for abducible in set_of_abducibles: 
                abducibles_str = abducibles_str + str(abducible) + "\n"
            after_P_str =  after_P_str + abducibles_str
        
        return after_P_str
    
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
    
    # run GUI
    window.show()
    sys.exit(app.exec_())