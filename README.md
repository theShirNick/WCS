# WCS Reasoner

## Download Executables

<div style="background-color:gray; border-radius:10px; padding:10px">
<a href="https://github.com/theShirNick/WCS/releases/download/v1.0.0/WCS.Reasoner.Apple.Silicon.dmg"><img src="https://upload.wikimedia.org/wikipedia/commons/3/30/MacOS_logo.svg" alt="Download for macOS (Apple Silicon)" width="100" height="100"></a> <a href="https://github.com/theShirNick/WCS/releases/download/v1.0.0/WCS.Reasoner.Intel.Mac.dmg"><img src="https://upload.wikimedia.org/wikipedia/commons/3/30/MacOS_logo.svg" alt="Download for macOS (Intel)" width="100" height="100"> </a> <a href="https://github.com/theShirNick/WCS/releases/download/v1.0.0/WCS.Reasoner.Windows.x86.zip"><img src="https://upload.wikimedia.org/wikipedia/commons/7/7e/Font_Awesome_5_brands_windows.svg" alt="Download for Windows" width="100" height="100"></a> <a href="https://github.com/theShirNick/WCS/releases/download/v1.0.0/WCS.Reasoner.Linux.x86.zip"><img src="https://upload.wikimedia.org/wikipedia/commons/d/df/Font_Awesome_5_brands_linux.svg" alt="Download for Linux" width="100" height="100"></a>
</div>



## How to run from code
Python 3.9.2 or newer is required (make sure PATH has path to Python and Python/Scripts)

### Homebrew (tested on arm64 Mac):
`brew install pyqt@5`

https://formulae.brew.sh/formula/pyqt@5


### Pip
Install all dependencies with pip:

`pip install pyqt5`

Run main.py with the appropriate Python interpreter:

`py main.py` or `python3 main.py` or `python3.9 main.py` etc.

If it doesn't run in a venv, try on the non-virtual Python

Linux may need `sudo apt-get install libxcb-xinerama0-dev`
