@echo off
echo Turn on the SRV-1 robot and then press a key to load the service.
echo Refresh the browser when DSS services are fully loaded.
echo.
pause
explorer "http://localhost:50000/drivedifferentialtwowheel"
..\..\..\..\bin\DssHost.exe -p:50000 -t:50001 -m:"..\..\..\..\Samples\Config\SimpleDashboard.manifest.xml" -m:"..\..\..\..\samples\config\Surveyor.SRV1.Vehicle.manifest.xml"
