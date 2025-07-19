#!/bin/bash

# Full paths to ensure the script uses the correct executables
DEFUDDLE_PATH=$(command -v defuddle)
PANDOC_PATH=$(command -v pandoc)

# Function to check if a command exists
command_exists() {
  command -v "$1" >/dev/null 2>&1
}

# Check if npm or pnpm is installed
if ! command_exists npm && ! command_exists pnpm; then
  echo "Neither npm nor pnpm is installed. Please install one of them first."
  exit 1
fi

# Install defuddle-cli if not already installed
if ! [ -x "$DEFUDDLE_PATH" ]; then
  if command_exists pnpm; then
    echo "Installing defuddle-cli using pnpm..."
    pnpm add -g defuddle-cli
  elif command_exists npm; then
    echo "Installing defuddle-cli using npm..."
    npm install -g defuddle-cli
  fi

  # Recheck the installation
  DEFUDDLE_PATH=$(command -v defuddle)
  if ! [ -x "$DEFUDDLE_PATH" ]; then
    echo "Failed to install defuddle-cli. Please check for errors."
    exit 1
  fi
fi

# Check if pandoc is installed
if ! [ -x "$PANDOC_PATH" ]; then
  echo "pandoc is not installed. Please install it first."
  exit 1
fi

# Check for URL parameter
if [ -z "$1" ]; then
  echo "Usage: $0 <url>"
  exit 1
fi

URL="$1"

# Temporary files for intermediate steps
MD_TEMP_FILE=$(mktemp)
ORG_TEMP_FILE=$(mktemp)

# Extract the webpage title
TITLE=$("$DEFUDDLE_PATH" parse "$URL" -p title)

# Check if title extraction was successful
if [ -z "$TITLE" ]; then
  echo "Failed to extract title from $URL"
  exit 1
fi

# Function to safely extract properties
safe_extract_property() {
  local url=$1
  local property=$2
  local value
  value=$("$DEFUDDLE_PATH" parse "$url" --property "$property" 2>/dev/null)
  if [[ "$value" != *"Error:"* && -n "$value" ]]; then
    echo "$value"
  else
    echo ""
  fi
}

# Extract additional properties safely
AUTHOR=$(safe_extract_property "$URL" "author")
DESCRIPTION=$(safe_extract_property "$URL" "description")
SOURCE=$(safe_extract_property "$URL" "source")

# Extract Markdown content
"$DEFUDDLE_PATH" parse "$URL" --md > "$MD_TEMP_FILE"

# Check if content extraction was successful
if [ ! -s "$MD_TEMP_FILE" ]; then
  echo "Failed to extract content from $URL"
  exit 1
fi

# Convert Markdown to org format using pandoc
"$PANDOC_PATH" --wrap=none -f markdown -t org "$MD_TEMP_FILE" -o "$ORG_TEMP_FILE"

# Check the org content for completeness
if [ ! -s "$ORG_TEMP_FILE" ]; then
  echo "Conversion to Org format failed"
  exit 1
fi

# Create org file header
ORG_HEADER="#+title: $TITLE\n"

# Create properties block only if any properties are present
if [ -n "$AUTHOR" ] || [ -n "$DESCRIPTION" ] || [ -n "$SOURCE" ]; then
  [ -n "$AUTHOR" ] && ORG_HEADER+=":author: $AUTHOR\n"
  [ -n "$DESCRIPTION" ] && ORG_HEADER+=":description: $DESCRIPTION\n"
  [ -n "$SOURCE" ] && ORG_HEADER+=":source: $SOURCE\n"
fi

# Determine output directory based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  OUTPUT_DIR="$HOME/Library/CloudStorage/Dropbox/org/denote/clipping/"
else
  # Assume Linux
  OUTPUT_DIR="$HOME/Dropbox/org/denote/clipping/"
fi

mkdir -p "$OUTPUT_DIR"  # Create directory if it doesn't exist

# Save the org content to the specified directory with a timestamp prefix
TIMESTAMP=$(date +"%Y%m%dT%H%M%S")
FILENAME="${OUTPUT_DIR}${TIMESTAMP}--${TITLE}__clipping.org"
{
  echo -e "$ORG_HEADER"
  cat "$ORG_TEMP_FILE"
} > "$FILENAME"

echo "Content saved to $FILENAME"

# Clean up temporary files
rm -f "$MD_TEMP_FILE" "$ORG_TEMP_FILE"
