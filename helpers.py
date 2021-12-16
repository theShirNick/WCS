import sys
import os

def resource_path(relative_path):
        """ Get absolute path to resource.
        Switch for running from source or for frozen PyInstaller instance."""
        try:
            # PyInstaller creates a temp folder and stores path in _MEIPASS
            base_path = sys._MEIPASS
        except Exception:
            base_path = os.path.abspath(".")
        returnMe = os.path.join(base_path, relative_path)
        return returnMe