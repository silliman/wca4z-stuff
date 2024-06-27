@echo off
"C:\Program Files\IBM Application Discovery and Delivery Intelligence\IBM Application Discovery Build Client\Bin\Release\IBMApplicationDiscoveryBuildClient.exe" "/np" "C:\Users\Administrator\wca4z-pilot-shortcut\clientCobol.ini"
echo Creating ClientCobol ADDI project
xcopy /s /y /i "C:\Users\Administrator\wca4z-pilot-shortcut\clientCOBOL\copybook" "C:\Users\Administrator\Desktop\WCA4Z-Data\refactoredCOBOL\refactoredCOBOL\copybook"
xcopy /s /y /i "C:\Users\Administrator\wca4z-pilot-shortcut\clientCOBOL\copybook" "C:\IBM AD\Mainframe Projects\clientCOBOL\Cobol Include"
xcopy /s /y /i "C:\Users\Administrator\wca4z-pilot-shortcut\clientCOBOL\cobol" "C:\IBM AD\Mainframe Projects\clientCOBOL\zOS Cobol"
echo Copied cobol files and copybooks
"C:\Program Files\IBM Application Discovery and Delivery Intelligence\IBM Application Discovery Build Client\Bin\Release\IBMApplicationDiscoveryBuildClient.exe" "/fb" "clientCobol"
echo Building clientCobol project...