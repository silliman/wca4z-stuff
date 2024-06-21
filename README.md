## Taking the Shortcuts

1. Clone this project in git bash terminal with:

	```
	git clone https://github.com/siler23/wca4z-pilot-shortcut.git
	```
2. Setup Refactoring assistant

	1. Login to TechZone on the Windows machine
	2. Get private IPs for refactoring assistant (3 out of 3) and windows instance (2 out of 3)
	3. Download the ssh key for your refactoring assistant instance (3 of 3)
	4. Open git bash terminal and type (replacing the xs with your actual ips):

		```
		windows_private_ip=xxx.xxx.xxx.xxx refactoring_private_ip=xxx.xxx.xx.xx "${HOME}/wca4z-pilot-automation/ipSetup.sh
		```
	5. Wait for the refactoring assistant started message in an opened PUTTY windows once the script finishes

3. Open ADDI and create new project named `clientCobol` following lab guide instructions

4. Copy cobol and copybooks in git bash terminal using:

	```
	"${HOME}/wca4z-pilot-automation/copyProcess.sh"
	```

5. Setup cloud watsonx instances following instructions in lab guide

6. Configure db2cli by opening git terminal and typing in credentials you've obtained for db2 in the form

	```
	host=db2hostname port=db2port user=db2user password=db2password "${HOME}/wca4z-pilot-automation/db2cli.sh"
	```

7. Copy and paste the outputted command in the command prompt

8. Follow lab guide instructions for creating `refactorCobol` ADDI project

9. Add Watsonx API key to vscode

System is now prepared for student use starting with refactoring assistant for lab student