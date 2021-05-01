import itertools
from interpretation import Interpretation
import sys
import os
import re


from PySide6.QtUiTools import QUiLoader
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QFile, QIODevice, Slot

from qt_material import apply_stylesheet

import itertools

from program import Program
from clauses import Clause, Rule, WC_Rule
from infix.expression import InfixExpression


if __name__ == "__main__":
    app = QApplication(sys.argv)

    ui_file_name = "ui/main.ui"
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
    QFontDatabase.addApplicationFont('ui/OverpassMono-Regular.ttf')
    apply_stylesheet(app, theme='dark_lightgreen.xml')

    # load custom style additions
    stylesheet = app.styleSheet()
    with open('ui/custom.css') as file:
        app.setStyleSheet(stylesheet + file.read().format(**os.environ))

    # force style 
    window.output_text_edit.setProperty('class', 'mono_font')
    window.rule_head_line_edit.setProperty('class', 'mono_font')
    window.rule_body_line_edit.setProperty('class', 'mono_font')
    window.clear_program_button.setProperty('class', 'danger')
    window.undo_button.setProperty('class', 'warning')
    
    atoms = dict()
    clauses = []
    program = Program(clauses)
    wc_program = Program([])


    # Rule Input
    @Slot()
    def add_rule():
        rule_body_text = InfixExpression( window.rule_body_line_edit.text(), atoms)
        rule_head_text = InfixExpression(window.rule_head_line_edit.text(), atoms)
        rule = Rule(rule_body_text, rule_head_text)
        clauses.append(rule)

        window.rule_body_line_edit.clear()
        window.rule_head_line_edit.clear()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(program))
    # Connect the Rule button to the function
    window.add_rule_button.clicked.connect(add_rule)


    # wcP Button
    @Slot()
    def wcP():
        global wc_program
        wc_program = program.weakly_complete()
        window.output_text_edit.clear()
        window.output_text_edit.appendPlainText(str(wc_program))

    # Connect the wcP button to the function
    window.wcP_button.clicked.connect(wcP)

    # Clear Program
    @Slot()
    def clear_program():
        clauses.clear()
        atoms.clear()
        window.output_text_edit.clear()

    # Connect the Clear button to the function
    window.clear_program_button.clicked.connect(clear_program)

    # Test "All False" Interpretation <<<<<<<<<< TODO remove
    @Slot()
    def test_all_false_interpretation():
        interp = Interpretation(set(), set(atoms.values()), set())
        window.output_text_edit.clear()
        if len(wc_program.clauses) == 0:
            if interp.isModel(program):
                window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊨ Is a model for\n{str(program)}")
            else:
                window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊭ Is not a model for\n{str(program)}")
        else:
            if interp.isModel(wc_program):
                window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊨ Is a model for\n{str(wc_program)}")
            else:
                window.output_text_edit.appendPlainText(f"{str(interp)}\n ⊭ Is not a model for\n{str(wc_program)}")

    # Connect the All False button to the function
    window.clever_button.clicked.connect(test_all_false_interpretation)

    
    # run GUI
    window.show()
    sys.exit(app.exec_())