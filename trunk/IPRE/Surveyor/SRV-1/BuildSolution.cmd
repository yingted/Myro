MSBuild Surveyor.SRV-1.sln /t:Rebuild /p:Configuration=Release /p:Platform="Any CPU"
del "..\..\..\..\store\contractDirectoryCache.bin"
Copy /Y "Srv1Services\Config\*.xml" "..\..\..\Config"
