#!/bin/bash
set -e

# Check if project name and Java version are provided
if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Please provide the project name and Java version as arguments."
  echo "Usage: ./deploy.sh <project_name> <java_version>"
  exit 1
fi

# Project name
PROJECT_NAME=$1

# Java version
JAVA_VERSION=$2

# Tomcat port (change this if your Tomcat uses a different port)
TOMCAT_PORT=8080

# Navigate to the project directory
cd ~/IdeaProjects/$PROJECT_NAME

# Set the Java version
jenv local $JAVA_VERSION

# 1. Package the application
mvn clean package -DskipTests

# 2. Copy the WAR file to Tomcat
sudo cp target/$PROJECT_NAME.war $TOMCAT_HOME/webapps/

# 3. Restart Tomcat
sudo $TOMCAT_HOME/bin/shutdown.sh || true
sleep 3
sudo $TOMCAT_HOME/bin/startup.sh

# Function to check if Tomcat is running
is_tomcat_running() {
  nc -z localhost $TOMCAT_PORT
}

# 4. Check if Tomcat started successfully, retry if necessary
if ! is_tomcat_running; then
  echo "Tomcat failed to start, retrying..."
  sleep 5
  $TOMCAT_HOME/bin/startup.sh
  sleep 5 # give some time to start again
  if ! is_tomcat_running; then
    echo "Tomcat failed to start after retrying. Please check the logs for more details."
    exit 1
  fi
fi

echo "Deployment successful and Tomcat is running."
