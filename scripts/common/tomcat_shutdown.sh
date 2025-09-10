#!/bin/bash

# Stop Tomcat
$TOMCAT_HOME/bin/shutdown.sh

# Function to check if Tomcat has stopped
check_tomcat_stopped() {
  local tomcat_pid=$(pgrep -f 'org.apache.catalina.startup.Bootstrap')
  if [ -z "$tomcat_pid" ]; then
    echo "Tomcat has successfully stopped."
  else
    echo "Tomcat failed to stop, attempting to force stop..."
    kill -9 $tomcat_pid
  fi
}

# Wait a few seconds and then check
sleep 5
check_tomcat_stopped
