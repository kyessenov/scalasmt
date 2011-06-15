#!/bin/bash
if [ "${SBT_JAR}" == "" ]; then 
  SBT_JAR="sbt-launch-0.7.7.jar"
fi 

java -Xmx512M -jar ${SBT_JAR} $@
