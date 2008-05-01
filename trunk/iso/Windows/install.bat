cd /d %0\..
msiexec /i python-2.4.4.msi
pyserial-2.2.win32.exe
pywin32-210.win32-py2.4.exe
pyTTS-3.0.win32-py2.4.exe
if "%allusersprofile%"=="C:\ProgramData" goto WinVista
@Echo Installing Sapi5Voices for WindowsXP
msiexec /i SAPI5VoiceInstaller.msi
:WinVista
pygame-1.7.1release.win32-py2.4.exe
myro-2.7.0.win32.exe
xmpppy-0.4.0.win32.exe
numpy-1.0.3.1.win32-py2.4.exe
PIL-1.1.6.win32-py2.4.exe
copy /Y snack\tkSnack.py c:\Python24\Lib\
copy /Y snack\snacklib\ c:\Python24\tcl\
copy /Y "Start Python.pyw"  "c:\Documents and Settings\%USERNAME%\Desktop\"
copy /Y misc\config-extensions.def c:\Python24\Lib\idlelib\
copy /Y misc\ScriptBinding.py c:\Python24\Lib\idlelib\
echo "Myro installation completed!"
pause
