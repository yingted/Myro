@echo off
echo Turn on the SRV-1 robot and then press a key to load the service.
echo Refresh the browser when DSS services are fully loaded.
echo.
pause
explorer "http://localhost:50000/camera/live"
..\..\..\..\bin\DssHost.exe -p:50000 -t:50001 -m:"..\..\..\..\samples\config\Surveyor.SRV1.Camera.manifest.xml"
