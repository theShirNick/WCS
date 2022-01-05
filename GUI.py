from PyQt5.QtWidgets import QMainWindow, QDialog, QLabel
from PyQt5.QtSvg import QSvgWidget
from PyQt5 import uic, QtCore
from helpers import *
from PyQt5.QtGui import QFontDatabase, QIcon

# Define windows and dialogs
class Main_window(QMainWindow):
    '''
        Main window of the WCR Reasoner app
    '''

    def __init__(self):
        super(Main_window, self).__init__()
        uic.loadUi(resource_path("resources/main.ui"), self)

class Starting_i_dialog(QDialog):
    '''
    Dialog of the WCR Reasoner app that prompts a starting interpretation for the semantic operator
    '''

    def __init__(self):
        super(Starting_i_dialog, self).__init__()
        uic.loadUi(resource_path("resources/starting_i_dialog.ui"), self)

class Halting_dialog(QDialog):
    '''
    Dialog of the WCR Reasoner app that appears when after 5 iterations of the semantic operator
    '''

    def __init__(self):
        super(Halting_dialog, self).__init__()
        uic.loadUi(resource_path("resources/halting_dialog.ui"), self)
        self.svg_widget = QSvgWidget(resource_path('resources/roundabout.svg'))
        self.svg_widget.setFixedSize(114,100)
        self.label = QLabel()
        self.label.setWordWrap(True)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.layout.addWidget(self.svg_widget,0,0,QtCore.Qt.AlignCenter)
        self.layout.addWidget(self.label,1,0)

def initialize_windows(app, window, starting_i_dialog):
    app.setWindowIcon(QIcon(resource_path("resources/icon.icns")))

    # Initial tabWidget state
    window.tabWidget.setTabEnabled(0, False)
    window.tabWidget.setTabEnabled(1, False)
    window.tabWidget.setTabEnabled(2, False)
    window.tabWidget.setTabEnabled(3, False)
    window.tabWidget.setTabVisible(4, True)
    window.tabWidget.setCurrentIndex(4)
    window.tabWidget.setTabVisible(5, False)

    QFontDatabase.addApplicationFont(resource_path('resources/OverpassMono-Regular.ttf'))

    # Attach css classes
    window.clear_program_button.setProperty('class', ['bot-right-rounded', 'bot-left-rounded'])
    window.open_guide_button.setProperty('class', ['bot-right-rounded', 'bot-left-rounded'])
    window.to_wc_button.setProperty('class', 'bot-left-rounded')
    window.p_latex.setProperty('class', 'bot-right-rounded')
    window.to_phi_button.setProperty('class', 'bot-left-rounded')
    window.wcP_latex.setProperty('class', 'bot-right-rounded')
    window.phi_latex.setProperty('class', 'bot-right-rounded')
    window.x_latex.setProperty('class', ['bot-right-rounded', 'bot-left-rounded'])
    

    #disable mac outline upon selecting a lineedit
    window.observation_line_edit.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    window.disjunction_line_edit_left.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    window.disjunction_line_edit_right.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    window.constraint_left_head_line_edit.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    window.constraint_right_body_line_edit.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    starting_i_dialog.truths.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    starting_i_dialog.falses.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)
    window.spinBox.setAttribute(QtCore.Qt.WA_MacShowFocusRect, 0)

    
    

    stylesheet = app.styleSheet()
    with open(resource_path('resources/custom.css')) as file:
        app.setStyleSheet(stylesheet + file.read().format(**os.environ))
    if sys.platform == 'win32' or sys.platform == 'linux':
        #Add extra CSS to make style consistent
        stylesheet = app.styleSheet()
        with open(resource_path('resources/non-mac.css')) as file:
            app.setStyleSheet(stylesheet + file.read().format(**os.environ))


    # error tab
    window.tabWidget.setTabVisible(5, False)

    help_text = '''Example of a valid program input:<br>
<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#D6CA86">fly</font> <font color="#89DCFB">X</font> if <font color="#D6CA86">bird</font> <font color="#89DCFB">X</font> and not <font color="#D6CA86">ab_fly</font> <font color="#89DCFB">X</font>;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#D6CA86">ab_fly</font> <font color="#89DCFB">X</font> if F;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#D6CA86">bird</font> <font color="#BBE491">tweety</font> if T;<br>
&nbsp;&nbsp;&nbsp;&nbsp; <font color="#D6CA86">bird</font> <font color="#BBE491">jerry</font> if T;<br>
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