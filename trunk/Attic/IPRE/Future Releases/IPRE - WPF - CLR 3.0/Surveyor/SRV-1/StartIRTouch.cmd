@echo off
echo Turn on the SRV-1 robot and then press a key to load the service.
echo Refresh the browser when DSS services are fully loaded.
echo.
pause
explorer "http://localhost:50000/srv1analogsensor"
..\..\..\..\bin\DssHost.exe -p:50000 -t:50001 -m:"..\..\..\..\Samples\Config\RoboticsTutorial3.manifest.xml" -m:"..\..\..\..\samples\config\Surveyor.SRV1.Vehicle.manifest.xml"
