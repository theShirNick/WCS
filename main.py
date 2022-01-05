import sys
import os, platform
import math
from collections import deque
import subprocess

from PyQt5 import QtCore
from PyQt5.QtWidgets import QApplication

from GUI import Main_window, Starting_i_dialog, Halting_dialog, initialize_windows

from helpers import resource_path
from truth_constant import TruthConstant
from interpretation import Interpretation
from program import Program
from clauses import Rule
from abductive_framework import Observation, get_set_of_abducibles, phi_with_abduction, skeptical
from infix.expression import InfixExpression
from phi import phi
from examples import Example
from ground import ground, find_vars_and_consts

if __name__ == "__main__":

    ########################################################## GUI ##########################################################
    QApplication.setAttribute(QtCore.Qt.AA_EnableHighDpiScaling) # possibly does nothing

    app = QApplication(sys.argv)
    # Initialize main window and dialog
    window = Main_window()
    starting_i_dialog = Starting_i_dialog()
    halting_dialog = Halting_dialog()
    # Attach css classes (more in the initialize_windows function)
    starting_i_dialog.truths.setProperty('class', 'top-left-rounded')
    starting_i_dialog.falses.setProperty('class', 'top-right-rounded')
    starting_i_dialog.submit_button.setProperty('class', ['bot-right-rounded', 'bot-left-rounded'])
    halting_dialog.halt_phi.setProperty('class', ['top-right-rounded', 'top-left-rounded'])
    halting_dialog.safety_off.setProperty('class', ['bot-right-rounded', 'bot-left-rounded'])
    initialize_windows(app, window, starting_i_dialog)
        

    ####################################################### App State #######################################################
    global ground_terms
    ground_terms:dict[str, TruthConstant] = dict() # a collection of terms with their values
    observations:set[Observation] = set() 
    clauses: set[Rule] = []
    program: Program = Program(clauses)
    ground_program:Program = Program([])
    wc_program:Program = Program([]) 
    set_of_abducibles:list[Rule] = list()
    interpretation_stack: deque[Interpretation] = deque()
    integrity_constraints: list[Rule] = list()
    variables: list[str] = list()
    consts: list[str] = list()
    is_OR = False
    phi_output = ''
    x_output = ''
    x_latex_output = ''


    ###################################################### Button Events ######################################################

    # Program Input button click
    def input_program():
        try: # invalid input causes a handeled exception displayed in the 'error' tab

            # disable later tabs
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setTabEnabled(1, False)
            window.tabWidget.setTabEnabled(2, False)
            window.tabWidget.setTabEnabled(3, False)
            window.tabWidget.setCurrentIndex(0)
            window.p_latex.setChecked(False)

            # 
            program_text = window.input_program_text_edit.toPlainText().replace(':-', '←')\
                            .replace('-:', '←')\
                            .replace(' if ', '←') # replacing 'if' with '←' is required for the app to work 

            if len(program_text) == 0:
                return
            if program_text[-1] == ';':
                program_text = program_text[:-1]
            split_clauses = program_text.split(';') # get list of Rules

            for clause in split_clauses:
                try:
                    left_head, right_body = clause.split('←', 1)
                except:
                    if len(str(clause)) == 0:
                        raise Exception(f"Empty clause found. Do you have an extra semicolon somwhere?")
                    raise Exception(f"All clauses must be of the form \"head if body\".\
                                    <br>This clause is wrong:<br>{clause} ")
                non_nec = False
                factual = False
                if '*' in left_head: # classification of contitionals
                    non_nec = True
                if '*' in right_body:
                    factual = True
                program.clauses.append(Rule(InfixExpression(left_head, ground_terms),
                                       InfixExpression(right_body, ground_terms), non_nec, factual))

            window.input_program_text_edit.setPlaceholderText("")
            P_output()
            window.input_program_text_edit.clear()

        
        except Exception as e:
            show_error(e)
    # Connect the Program button to the function
    window.input_program_button.clicked.connect(input_program)
    
    # Observation Input
    def input_observation():
        try:
            # disable later tabs
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabEnabled(1, False)
            window.tabWidget.setTabEnabled(2, False)
            window.tabWidget.setTabEnabled(3, False)
            
            window.p_latex.setChecked(False)

            if window.observation_line_edit.text() != "":
                observation_expr = InfixExpression(window.observation_line_edit.text(), ground_terms)
                observations.add(Observation(observation_expr))

                window.observation_line_edit.clear()
           
            P_output()
                
        except Exception as e:
            show_error(e)

    # Connect the Observation button to the function
    window.input_observation_button.clicked.connect(input_observation)

    # Integrity Constraint Input
    def input_IC():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabEnabled(1, False)
            window.tabWidget.setTabEnabled(2, False)
            window.tabWidget.setTabEnabled(3, False)
            window.p_latex.setChecked(False)

            if window.constraint_right_body_line_edit.text() != '' and\
            window.constraint_left_head_line_edit.text() != '':
                constraint_right_body_expr = InfixExpression(window.constraint_right_body_line_edit.text(), ground_terms)
                constraint_left_head_expr = InfixExpression(window.constraint_left_head_line_edit.text(), ground_terms)
                constraint = Rule(constraint_left_head_expr, constraint_right_body_expr)
                integrity_constraints.append(constraint)

                window.constraint_right_body_line_edit.clear()
                window.constraint_left_head_line_edit.clear()

            P_output()
                
        except Exception as e:
            show_error(e)
            
    # Connect the Integrity Constraint button to the function
    window.input_constraint_button.clicked.connect(input_IC)

    # Disjunction Input button click
    def input_disjunction():
        try:
            window.tabWidget.setTabEnabled(0, True)
            window.tabWidget.setCurrentIndex(0)
            window.tabWidget.setTabEnabled(1, False)
            window.tabWidget.setTabEnabled(2, False)
            window.tabWidget.setTabEnabled(3, False)
            
            window.p_latex.setChecked(False)

            # get text from input fields
            left_text = window.disjunction_line_edit_left.text()
            right_text = window.disjunction_line_edit_right.text()
            if left_text != '' and right_text != '':
                # construct OR or XOR constraint
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
    # Connect the Disjunction button to the function
    window.input_disjunction_button.clicked.connect(input_disjunction)

    # 'Ground and Weakly Complete' button click 
    def wcP():
        try:
            window.tabWidget.setTabEnabled(1, True) 
            window.tabWidget.setCurrentIndex(1)
            window.wcP_latex.setChecked(False)
            global program
            global ground_program
            global integrity_constraints

            # detect vars and consts in the abductive framework
            find_vars_and_consts(program.clauses, variables, consts)
            find_vars_and_consts(integrity_constraints, variables, consts)

            # ground the program
            ground_program = Program(ground(program.clauses, variables, consts))
            integrity_constraints = ground(integrity_constraints, variables, consts)

            # get abducibles
            global set_of_abducibles
            set_of_abducibles = get_set_of_abducibles(ground_terms, ground_program)

            # Set a recomended cap on the length of explanations. 
            unique_abducible_heads = set()
            for abducible in set_of_abducibles:
                unique_abducible_heads.add(abducible.left_head)
            default_cap = 0
            combination_count = 0

            if len(set_of_abducibles) > 0:
                window.spinBox.setVisible(True)
                while combination_count < 10000 and\
                    default_cap <= len(unique_abducible_heads)\
                    and default_cap < len(set_of_abducibles):
                    default_cap = default_cap+1
                    combination_count = combination_count + \
                                        (math.factorial(len(set_of_abducibles))/ \
                                        (math.factorial(default_cap) * math.factorial(len(set_of_abducibles)\
                                        -default_cap)))
                    
                window.spinBox.setMinimum(1)
                window.spinBox.setMaximum(len(unique_abducible_heads))
                window.spinBox.setValue(default_cap-1)
            else:
                window.spinBox.setVisible(False)

            global wc_program
            wc_program = ground_program.weakly_complete()

            P_output(True) # call with a wcFlag

        except Exception as e:
            show_error(e)
    # Connect the 'Ground and Weakly Complete' button to the function
    window.to_wc_button.clicked.connect(wcP)

    # Explanation length changed
    def X_len_warning():
        '''
        Indicate a yellow or red warning depending on the number of possible explanations that will be generated with this length
        '''
        combination_count = 0
        val = window.spinBox.value()
        unique_abducible_heads = set()
        for abducible in set_of_abducibles:
            unique_abducible_heads.add(abducible.left_head)
        
        # sum of combinations
        for i in range(1, val+1):
            combination_count = combination_count + (math.factorial(len(set_of_abducibles))/(math.factorial(i) * math.factorial(len(set_of_abducibles)- i)))

        if combination_count < 10000:
            window.spinBox.setStyleSheet("QSpinBox:focus{border-bottom: 2px solid rgba(0,0,0,0); color:rgb(255, 255, 255); } QSpinBox:focus{border-bottom: 2px solid rgb(90, 86, 95);}")
            window.spinBox.setToolTip('Maximum explanation length')
        elif combination_count >= 10000 and combination_count <= 400000:
            window.spinBox.setStyleSheet("QSpinBox{border-bottom: 2px solid rgba(255,255,0, 0.6); color:rgb(255, 255, 255);}")
            window.spinBox.setToolTip(f'Maximum explanation length.\nIt can take a few seconds to crunch {int(combination_count)} explanations')
        else:
            window.spinBox.setStyleSheet("QSpinBox{border-bottom: 2px solid rgba(255,0,0,0.4); color:rgb(255, 255, 255);}")
            window.spinBox.setToolTip(f'Maximum explanation length.\nIt can take a while to crunch {int(combination_count)} explanations')
    # Connect the explanation length change event to the function
    window.spinBox.valueChanged.connect(X_len_warning)

    # Enter starting interpretation button clicked
    def show_starting_i_dialog():
        starting_i_dialog.exec_()
    # Connect the 'Enter starting_i' button to the function
    window.starting_i_button.clicked.connect(show_starting_i_dialog)

    # Sumbit starting interpretation buttion clicked
    def submit_starting_i_dialog():
        truths_input = set()
        if len(starting_i_dialog.truths.text()) > 0:
            truths_input = starting_i_dialog.truths.text().split(',')
            for i in range(len(truths_input)):
                truths_input[i] = str(InfixExpression(truths_input[i], ground_terms))

        falses_input = set()
        if len(starting_i_dialog.falses.text()) > 0:
            falses_input = starting_i_dialog.falses.text().split(',')
            for i in range(len(falses_input)):
                falses_input[i] = str(InfixExpression(falses_input[i], ground_terms))
        
        unknowns_left = set()
        for gt in ground_terms:
            if gt not in falses_input and gt not in truths_input:
                unknowns_left.add(gt)
        
        starting_i = Interpretation(ground_terms, set(truths_input), set(falses_input), unknowns_left)
        
        # reset dialog
        interpretation_stack.clear()
        interpretation_stack.append(starting_i)
        starting_i_dialog.truths.clear()
        starting_i_dialog.falses.clear()
        starting_i_dialog.close()

        # run the semantic operator on the current interpretation
        phi_fixed_point()

    # Connect the 'Enter starting_i' button to the function
    starting_i_dialog.submit_button.clicked.connect(submit_starting_i_dialog)

    #Close the halting dialog and stop 
    def halt_phi():
        halting_dialog.done(0)    
    halting_dialog.halt_phi.clicked.connect(halt_phi)

    #Close the halting dialog and run 5 more iterations 
    def more_phi():
        halting_dialog.done(1)
    halting_dialog.more_phi.clicked.connect(more_phi)

    #Close the halting dialog and keep going untill fixed point (infinite loop possible)
    def safety_off():
        halting_dialog.done(2)
    halting_dialog.safety_off.clicked.connect(safety_off)

    # Semantic Operator Phi button clicked 
    def phi_fixed_point():
        try:
            window.tabWidget.setTabEnabled(2, True)
            window.tabWidget.setTabEnabled(3, False)
            window.to_x_button.setEnabled(True)
            window.tabWidget.setCurrentIndex(2)
            window.phi_latex.setChecked(False)
            window.PhiTextEdit.setFocus()
            output = ''
            window.PhiTextEdit.clear()

            if len(interpretation_stack) == 0:
                #start with an empty interpretation, i.e. all terms are unknown
                interpretation_stack.append(Interpretation(ground_terms, set(), set(), set(ground_terms.keys())))
            
            # prompt the user to halt the Phi operator after 5 interations
            halt_safety=True
            iterations_left = 5
            fixed_point_found = False # True if the next interpretation is the same as the current one

            while fixed_point_found == False and (iterations_left > 0 or halt_safety == False):
                output = output + f"Φ↑{len(interpretation_stack) -1}: {str(interpretation_stack[-1])}<hr>"
                next_phi = phi(wc_program, interpretation_stack[-1]) # run one iteration of the semantic opertator
                if interpretation_stack[-1] == next_phi:
                    output = output + f"Fixed point found.<br>"
                    fixed_point_found = True

                    # Check if integrity constraints are satisfied.
                    integrity_constraint_check = True 
                    for constraint in integrity_constraints:
                        if constraint.evaluate() != TruthConstant.TRUE:
                            integrity_constraint_check = False
                            break
                    if not integrity_constraint_check:
                        output = output + f"Integrity constraint not satisfied. Try abduction.<br>"

                    # Check if observations are explained
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

                global phi_output
                phi_output = window.PhiTextEdit.toHtml() 
                iterations_left = iterations_left - 1
                if iterations_left == 0 and fixed_point_found == False and halt_safety == True:
                    halting_dialog.label.setText(
                        f"The semantic operator has been running for {len(interpretation_stack)-1} iterations. \nIt may never finish. ")
                    QApplication.beep()
                    halting_decision = halting_dialog.exec()
                    if halting_decision == 0:
                        window.PhiTextEdit.setHtml(output + "...") # The there dots are used as a trigger for LaTeX \vdots 
                        phi_output = window.PhiTextEdit.toHtml()
                        window.tabWidget.setTabEnabled(3, False)
                        window.to_x_button.setEnabled(False)
                    elif halting_decision == 1:
                        iterations_left = 5
                    elif halting_decision == 2:
                        halt_safety = False
        except Exception as e:
            show_error(e)
    # 'Next' on the wcP tab clicked        
    def start_phi_from_empty_interpretation():
        interpretation_stack.clear()
        phi_fixed_point()
    # Connect the 'Find Least Fixed Point' button to the function
    window.to_phi_button.clicked.connect(start_phi_from_empty_interpretation)
    

    # 𝒳 - explain with abduction
    def abduction():  
        window.tabWidget.setTabEnabled(3, True)
        window.tabWidget.setCurrentIndex(3)
        window.x_latex.setChecked(False)

        global x_output # HTML
        x_output = ''
        global x_latex_output # LaTeX
        x_latex_output = ''
        
        # Output text for an empty set of abducibles
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
                x_output = x_output + f"Skeptically, nothing new follows. We already know:<br>{old_s}<br>"
            elif len(interpretation_stack) == 1:
                x_output = x_output + f"Empty fixed point, empty set of abducibles. Nothing follows."
            else:
                raise Exception("Interpretation stack empty. Did Phi run correctly?")

        else:
            # Find valid minimal explanations
            explanations_interpretations = phi_with_abduction(set_of_abducibles,
                                                              window.spinBox.value(),
                                                              ground_program,
                                                              observations,
                                                              interpretation_stack[-1],
                                                              integrity_constraints)
            if len(explanations_interpretations) > 0:
                abduced_interpretations = list() 
                for expl, interpr in explanations_interpretations:
                    x_output = x_output + f"𝒳 {expl}<br>yields minimal model<br>{interpr}<hr>"
                    abduced_interpretations.append(interpr)

                    x_latex = r'\noindent $\mathcal{X}$: \{'
                    for rule in expl:
                        x_latex = x_latex + rule.latex()[:-6] + ' '
                    if len(expl) > 0:
                        x_latex = x_latex[:-2] 
                    x_latex = x_latex + r'\}\\<br>'
                    x_latex_output = x_latex_output + x_latex.replace(r'_', r'\_') + \
                                     r"\noindent $\mathcal{M}_{\mathcal{P}\cup\mathcal{X}}$ = " +  \
                                     interpr.line_latex().replace(r'_', r'\_') + \
                                     r'\\<hr>'
                        
                skeptical_result = skeptical(ground_terms, ground_program, abduced_interpretations)
                x_output = skeptical_result + x_output
            else:
                x_output = "Abduction yielded nothing"
              
        window.XTextEdit.setHtml(x_output)
        window.XTextEdit.setFocus()
        
    # Connect the 'Explain with Skeptical Abduction' button to the function
    window.to_x_button.clicked.connect(abduction)
        
    # Exclusive disjunction button
    def XOR_switch():
        global is_OR
        is_OR = window.exclusive_button.isChecked()
        if is_OR:
            window.exclusive_button.setText("OR")
        else:
            window.exclusive_button.setText("XOR")  
    # Connect the XOR switch to the function  
    window.exclusive_button.clicked.connect(XOR_switch)


    # P Latex output switch button
    def P_latex_switch():
        if window.p_latex.isChecked():
            P_latex_output()
        else:
            P_output()
    # Connect the LaTeX switch to the function  
    window.p_latex.clicked.connect(P_latex_switch)

    # wcP Latex output switch button
    def wcP_latex_switch():
        if window.wcP_latex.isChecked():
            P_latex_output(True)
        else:
            P_output(True)
    # Connect the LaTeX switch to the function  
    window.wcP_latex.clicked.connect(wcP_latex_switch)

    # Ф Latex output switch button
    def Phi_latex_switch():
        if window.phi_latex.isChecked():
            output = r'%\usepackage{array}<br>\newcolumntype{M}[1]{>{\centering\arraybackslash}m{#1}}<br>\begin{center}<br>\begin{tabular}{ M{0.5cm} M{3cm} M{3cm} }<br>\hline<br>$\Phi$ & $I^\top$ & $I^\bot$ \\<br>\hline<br>'
            for i in range(len(interpretation_stack)):
                output = output + f"$\\uparrow${str(i)} & {interpretation_stack[i].table_latex()} \\\\<br>\\hline<br>"
            output = output.replace('_', r'\_')
            if '...' in phi_output:
                output = output + r"~\vdots & ~\vdots & ~\vdots \\<br>\hline<br>"
            window.PhiTextEdit.setHtml(output+"\\end{tabular}<br>\\end{center}")
        else:
            window.PhiTextEdit.setHtml(phi_output)
        window.PhiTextEdit.setFocus()
            
    # Connect the LaTeX switch to the function  
    window.phi_latex.clicked.connect(Phi_latex_switch)

    # 𝒳 Latex output switch button
    def X_latex_switch():
        if window.x_latex.isChecked():
            window.XTextEdit.setHtml(x_latex_output)
        else:
            window.XTextEdit.setHtml(x_output)
            
    # Connect the LaTeX switch to the function  
    window.x_latex.clicked.connect(X_latex_switch)

    def P_latex_output(wcFlag = False):
        '''
            Construct and output LaTeX containing the program, observations, integrity constraints, and abducibles

            Slight differences between the P tab and the wcP tab.
        '''
        
        output = ''

        if not wcFlag:
            output = r"\noindent $\mathcal{P}$:\\<br>" + program.latex()
        elif  wcFlag:
            output = output + r"\noindent $g\mathcal{P}$:\\<br>"+ program.latex() + r'<hr>\noindent wc$\mathcal{P}$:\\<br>' + wc_program.latex()
           
        if len (observations) > 0:
            observations_str = r"<hr>\noindent $\mathcal{O}$:~\{"
            for observation in observations:
                observations_str = observations_str + observation.latex()[:-1] + r',~'
            observations_str = observations_str[:-2]
            output =  output + observations_str + r"\}"

        if len (integrity_constraints) > 0:
            IC_string = r"<hr>\noindent $\mathcal{I}$$\mathcal{C}$:\\"
            for IC in integrity_constraints:
                IC_string = IC_string + IC.latex()
            IC_string = IC_string[:-7] + r'\}'
            output =  output + IC_string

        if len(set_of_abducibles) > 0 and wcFlag == True:
            abducibles_str = r"<hr>\noindent $\mathcal{A}$:\\"
            for abducible in set_of_abducibles: 
                abducibles_str = abducibles_str + abducible.latex()
            abducibles_str = abducibles_str[:-7] + r'.'
            output=  output + abducibles_str
            

        output = output.replace("_", "\\_")
        if not wcFlag:
            window.PTextEdit.clear()
            window.PTextEdit.setHtml(output)
        elif wcFlag:
            window.wcPTextEdit.clear()
            window.wcPTextEdit.setHtml(output)
    
    
    def P_output(wcFlag = False):
        '''
            Construct and output HTML text containing the program, observations, integrity constraints, and abducibles

            Slight differences between the P tab and the wcP tab.
        '''
        
        output = ''

        if not wcFlag:
            output = output + "𝒫:<br>"+ str(program)
        elif  wcFlag:
            output = output + "𝑔𝒫:<br>"+ str(ground_program) + '<hr>𝓌𝒸𝒫:<br>' + str(wc_program)
           
        if len (observations) > 0:
            observations_str = "<hr>𝒪:<br>"
            for observation in observations:
                observations_str = observations_str + str(observation) + ';<br>'
            observations_str = observations_str[:-5]
            output =  output + observations_str

        if len (integrity_constraints) > 0:
            IC_string = "<hr>ℐ𝒞:<br>"
            for IC in integrity_constraints:
                IC_string = IC_string + str(IC) + ';<br>'
            IC_string = IC_string[:-5]
            output =  output + IC_string

        if len(set_of_abducibles) > 0 and wcFlag == True:
            abducibles_str = "<hr>𝒜:<br>"
            for abducible in set_of_abducibles: 
                abducibles_str = abducibles_str + str(abducible) + ";<br>"
            abducibles_str = abducibles_str[:-5]
            output=  output + abducibles_str
        
        if not wcFlag:
            window.PTextEdit.clear()
            window.PTextEdit.setHtml(output + "")
            window.PTextEdit.setFocus()
        elif wcFlag:
            window.wcPTextEdit.clear()
            window.wcPTextEdit.setHtml(output + "")
            window.wcPTextEdit.setFocus()
    
    def show_error(e):
            window.tabWidget.setTabVisible(5, True)
            window.tabWidget.setCurrentIndex(5)
            window.error_textEdit.clear()
            window.error_textEdit.append(str(e))

    # Open guide.pdf
    def open_guide():
        if platform.system() == 'Darwin':       # macOS
            subprocess.call(('open', resource_path("resources/guide.pdf")))
        elif platform.system() == 'Windows':    # Windows
            os.startfile(resource_path("resources/guide.pdf")) 
        else:                                   # linux variants
            subprocess.call(('xdg-open', resource_path("resources/guide.pdf")))
    # Connect the Open Guide button to the function
    window.open_guide_button.clicked.connect(open_guide)


    # Clear Program
    def clear_program():
        '''
        Reset the app state
        '''

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
        global starting_i
        starting_i = None
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
        window.p_latex.setChecked(False)
        global is_OR
        if is_OR:
            window.exclusive_button.nextCheckState()
            is_OR = False
            XOR_switch()
    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    ####################################################### Run Examples #######################################################
    def _1_AA():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._1_AA_PROGRAM.value)
        input_program()
    window.action1_AA.triggered.connect(_1_AA)

    def _2_AA_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._2_AA_ALT_PROGRAM.value)
        input_program()
    window.action2_AA_ALT.triggered.connect(_2_AA_ALT)
    
    def _3_AA_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._3_AA_ADD_PROGRAM.value)
        input_program()
    window.action3_AA_ADD.triggered.connect(_3_AA_ADD)

    def _4_DA():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._4_DA_PROGRAM.value)
        input_program()
    window.action4_DA.triggered.connect(_4_DA)

    def _5_DA_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._5_DA_ALT_PROGRAM.value)
        input_program()
    window.action5_DA_ALT.triggered.connect(_5_DA_ALT)
    
    def _6_DA_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._6_DA_ADD_PROGRAM.value)
        input_program()
    window.action6_DA_ADD.triggered.connect(_6_DA_ADD)

    def _7_AC():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._7_AC_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._7_AC_OBSERVATION.value)
        input_observation()
    window.action7_AC.triggered.connect(_7_AC)
    
    def _8_AC_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._8_AC_ALT_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._8_AC_ALT_OBSERVATION.value)
        input_observation()
    window.action8_AC_ALT.triggered.connect(_8_AC_ALT)

    def _9_AC_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._9_AC_ADD_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._9_AC_ADD_OBSERVATION.value)
        input_observation()
    window.action9_AC_ADD.triggered.connect(_9_AC_ADD)

    def _10_DC():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._10_DC_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._10_DC_OBSERVATION.value)
        input_observation()
    window.action10_DC.triggered.connect(_10_DC)

    def _11_DC_ALT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._11_DC_ALT_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._11_DC_ALT_OBSERVATION.value)
        input_observation()
    window.action11_DC_ALT.triggered.connect(_11_DC_ALT)

    def _12_DC_ADD():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example._12_DC_ADD_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example._12_DC_ADD_OBSERVATION.value)
        input_observation()
    window.action12_DC_ADD.triggered.connect(_12_DC_ADD)
    
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

    def CLASS_1():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_1_PROGRAM.value)
        input_program()
    window.action1_AA_both.triggered.connect(CLASS_1)

    def CLASS_2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_2_PROGRAM.value)
        input_program()
    window.action2_DA_both.triggered.connect(CLASS_2)

    def CLASS_3():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_3_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.CLASS_3_OBSERVATION.value)
        input_observation()
    window.action3_AC_non_necessary.triggered.connect(CLASS_3)

    def CLASS_4():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CLASS_4_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.CLASS_4_OBSERVATION.value)
        input_observation()
    window.action4_DC_factual.triggered.connect(CLASS_4)

    def DATALOG_1():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DATALOG_1_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.DATALOG_1_OBSERVATION.value)
        input_observation()
    window.action1_Birds_Explicit.triggered.connect(DATALOG_1)

    def DATALOG_2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.DATALOG_2_PROGRAM.value)
        input_program()
        window.observation_line_edit.clear()
        window.observation_line_edit.setText(Example.DATALOG_2_OBSERVATION.value)
        input_observation()
    window.action2_Birds_Implicit.triggered.connect(DATALOG_2)

    def IA2():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.IA2.value)
        input_program()
    window.action1_IA2.triggered.connect(IA2)

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

    def CONTEXT_TWEETY_DEFAULT():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CONTEXT_TWEETY_DEFAULT_PROGRAM.value)
        input_program()
    window.actionTweety_by_Default.triggered.connect(CONTEXT_TWEETY_DEFAULT)

    def CONTEXT_TWEETY_PENGUIN():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CONTEXT_TWEETY_PENGUIN_PROGRAM.value)
        input_program()
    window.actionTweety_is_a_Penguin.triggered.connect(CONTEXT_TWEETY_PENGUIN)

    def CONTEXT_TWEETY_HAS_WINGS():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CONTEXT_TWEETY_HAS_WINNGS_PROGRAM.value)
        input_program()
    window.actionTweety_has_Wings.triggered.connect(CONTEXT_TWEETY_HAS_WINGS)

    def CONTEXT_NON_MONOTONIC():
        clear_program()
        window.input_program_text_edit.clear()
        window.input_program_text_edit.setPlainText(Example.CONTEXT_NON_MONOTONIC_PROGRAM.value)
        input_program()
    window.actionNon_monotonic.triggered.connect(CONTEXT_NON_MONOTONIC)

    ####################################################### Run GUI #######################################################
    window.show()
    sys.exit(app.exec_())