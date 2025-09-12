#!/bin/bash
set -e

# Check if project name and Java version are provided
if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Please provide the project name and Java version as arguments."
  echo "Usage: ./deploy.sh <project_name> <java_version>"
  exit 1
fi

PROJECT_NAME=$1

JAVA_VERSION=$2

TOMCAT_PORT=8080

cd ~/IdeaProjects/$PROJECT_NAME

jenv local $JAVA_VERSION

mvn clean package -DskipTests

rm -rf /opt/homebrew/Cellar/tomcat@9/9.0.109/libexec/webapps/$PROJECT_NAME
rm -f /opt/homebrew/Cellar/tomcat@9/9.0.109/libexec/webapps/$PROJECT_NAME.war

cp target/$PROJECT_NAME.war $TOMCAT_HOME/webapps/

$TOMCAT_HOME/bin/shutdown.sh || true
sleep 3
$TOMCAT_HOME/bin/startup.sh

is_tomcat_running() {
  nc -z localhost $TOMCAT_PORT
}

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
