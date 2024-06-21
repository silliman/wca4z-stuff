#!/usr/bin/env bash
if [ -z ${host} ]
then
  echo "Usage: host=db2hostname port=db2port user=db2user password=db2password ./db2cli.sh"
  exit 1
fi
if [ -z ${port} ]
then
  echo "Usage: host=db2hostname port=db2port user=db2user password=db2password ./db2cli.sh"
  exit 1
fi
if [ -z ${user} ]
then
  echo "Usage: host=db2hostname port=db2port user=db2user password=db2password ./db2cli.sh"
  exit 1
fi
if [ -z ${password} ]
then
  echo "Usage: host=db2hostname port=db2port user=db2user password=db2password ./db2cli.sh"
  exit 1
fi


db2cli writecfg add -database bludb -host "${host}" -port "${port}"
db2cli writecfg add -dsn dashdb -database bludb -host "${host}" -port "${port}"
db2cli writecfg add -database bludb -host "${host}" -port "${port}" -parameter "SecurityTransportMode=SSL"
echo -e "Copy and paste the following line into a command prompt\n\nC:\Users\Administrator\Desktop\db2configuration.bat dbName="wca4zdb" db2Host="${host}" db2Port="${port}" db2User="${user}" db2Password="${password}" useTLS=true"
