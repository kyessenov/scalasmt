#!/bin/bash
if [ "${SBT_JAR}" == "" ]; then 
  SBT_JAR="sbt-launch-0.7.5.jar"
fi 

java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx512M -jar ${SBT_JAR} $@
