# -*- mode: python ; coding: utf-8 -*-


block_cipher = None


a = Analysis(['main.py'],
             pathex=[],
             binaries=[],
             datas=[("resources/starting_i_dialog.ui", "resources"),
                    ("resources/halting_dialog.ui", "resources"),
                    ("resources/main.ui", "resources"),
                    ("resources/OverpassMono-Regular.ttf", "resources"),
                    ("resources/OFL.txt", "resources"),
                    ("resources/roundabout.svg", "resources"),
                    ("resources/icon.icns", "resources"),
                    ("resources/icon.ico", "resources"),
                    ("resources/non-mac.css", "resources"),    
                    ("resources/custom.css", "resources"),
                    ("resources/guide.pdf", "resources")],
             hiddenimports=[],
             hookspath=[],
             hooksconfig={},
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher,
             noarchive=False)
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)

exe = EXE(pyz,
          a.scripts, 
          [],
          exclude_binaries=True,
          icon='resources/icon.ico',
          name='WCS Reasoner',
          debug=False,
          bootloader_ignore_signals=False,
          strip=False,
          upx=True,
          console=False,
          disable_windowed_traceback=False,
          target_arch=None,
          codesign_identity=None,
          entitlements_file=None )
coll = COLLECT(exe,
               a.binaries,
               a.zipfiles,
               a.datas, 
               strip=False,
               upx=True,
               upx_exclude=[],
               name='WCS Reasoner')
app = BUNDLE(coll,
             name='WCS Reasoner.app',
             icon='resources/icon.icns',
             bundle_identifier=None)
