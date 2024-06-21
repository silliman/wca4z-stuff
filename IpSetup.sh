#!/usr/bin/env bash
if [ -z ${windows_private_ip} ]
then
  echo "Usage: windows_private_ip=xxx.xxx.xxx.xxx refactoring_private_ip=xxx.xxx.xx.xx ./ipSetup.sh"
  exit 1
fi
if [ -z ${refactoring_private_ip} ]
then
  echo "Usage: windows_private_ip=xxx.xxx.xxx.xxx refactoring_private_ip=xxx.xxx.xx.xx ./ipSetup.sh"
  exit 1
fi

echo ${refactoring_private_ip} | "${HOME}/Desktop/Change-hosts.bat"
echo "Privata ip set for refcactoring assistant on Windows host"
ssh -i ~/Downloads/pem_ibmcloudvsi_download.pem itzuser@wca4z-ra.ibm.com -p 2223 "sudo -S echo ${windows_private_ip} | sudo -S ./Change-host-ip.sh && sudo -S chmod 755 /etc/hosts"
echo "Private ip set for Windows host on refactoring assistant"
putty.exe -load "wca4z-ra"