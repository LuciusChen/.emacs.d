#!/bin/bash

# Full path to ensure the script uses the correct pandoc executable
PANDOC_PATH=$(command -v pandoc)

# Check if pandoc is installed
if ! [ -x "$PANDOC_PATH" ]; then
  echo "pandoc is not installed. Please install it first."
  exit 1
fi

# Check for input Markdown file parameter
if [ -z "$1" ]; then
  echo "Usage: $0 <input_markdown_file>"
  exit 1
fi

INPUT_MD_FILE="$1"
BASENAME=$(basename "$INPUT_MD_FILE" .md) # Extract the base file name without extension
ORG_TEMP_FILE=$(mktemp)

# Convert Markdown to org format using pandoc
"$PANDOC_PATH" --wrap=none -f markdown -t org "$INPUT_MD_FILE" -o "$ORG_TEMP_FILE"

# Check the org content for completeness
if [ ! -s "$ORG_TEMP_FILE" ]; then
  echo "Conversion to Org format failed"
  exit 1
fi

# Extract metadata from the YAML front matter
TITLE=$(sed -n 's/^title: *\(.*\)/\1/p' "$INPUT_MD_FILE")
AUTHOR=$(sed -n 's/^author: *\(.*\)/\1/p' "$INPUT_MD_FILE")
DESCRIPTION=$(sed -n 's/^description: *\(.*\)/\1/p' "$INPUT_MD_FILE")

# Determine output directory based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  OUTPUT_DIR="$HOME/Library/CloudStorage/Dropbox/org/denote/clipping/"
else
  # Assume Linux
  OUTPUT_DIR="$HOME/Dropbox/org/denote/clipping/"
fi

mkdir -p "$OUTPUT_DIR"  # Create directory if it doesn't exist

# Prepare Org file header
ORG_HEADER="#+title: $TITLE\n"
[ -n "$AUTHOR" ] && ORG_HEADER+=":author: $AUTHOR\n"
[ -n "$DESCRIPTION" ] && ORG_HEADER+=":description: $DESCRIPTION\n"

# Add the header to the Org file
FILENAME="${OUTPUT_DIR}${BASENAME}.org"
{
  echo -e "$ORG_HEADER"
  cat "$ORG_TEMP_FILE"
} > "$FILENAME"

echo "Content saved to $FILENAME"
