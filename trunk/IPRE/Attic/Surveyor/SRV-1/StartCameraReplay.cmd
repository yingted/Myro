@echo off
echo Refresh the browser when DSS services are fully loaded.
echo Then click on "WebCam Replay" on the left pane.
echo.
pause
explorer "http://localhost:50000"
..\..\..\..\bin\DssHost.exe -p:50000 -t:50001 -m:"..\..\..\..\samples\config\WebCamReplay.manifest.xml"
