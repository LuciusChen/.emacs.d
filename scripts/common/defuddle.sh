#!/usr/bin/env bash

set -euo pipefail

command_exists() {
  command -v "$1" >/dev/null 2>&1
}

usage() {
  cat <<'EOF'
Usage: defuddle.sh [--stdout] [--output FILE | --output-dir DIR] <source>

Convert a URL or local HTML file to Org using defuddle + pandoc.

Options:
  --stdout           Write Org to stdout instead of a file
  --output FILE      Write Org to FILE
  --output-dir DIR   Write an auto-named Org file into DIR
  -h, --help         Show this help

If no output option is given, the script keeps the old behavior and writes
to the default denote clipping directory.
EOF
}

default_output_dir() {
  if [[ "${OSTYPE:-}" == darwin* ]]; then
    printf '%s\n' "$HOME/Library/CloudStorage/Dropbox/org/denote/clipping/"
  else
    printf '%s\n' "$HOME/Dropbox/org/denote/clipping/"
  fi
}

sanitize_filename_component() {
  local value="$1"
  value=${value//$'\n'/ }
  value=${value//$'\r'/ }
  value=${value//$'\t'/ }
  value=$(printf '%s' "$value" | sed 's#[/:*?"<>|\\]#-#g; s/[[:space:]]\{1,\}/ /g; s/^ //; s/ $//')
  if [[ -z "$value" ]]; then
    value="untitled"
  fi
  printf '%s\n' "$value"
}

json_get() {
  local file="$1"
  local key="$2"
  node -e '
const fs = require("fs");
const [file, key] = process.argv.slice(1);
const data = JSON.parse(fs.readFileSync(file, "utf8"));
const value = data[key];
if (value === undefined || value === null) {
  process.exit(0);
}
if (typeof value === "string") {
  process.stdout.write(value);
} else {
  process.stdout.write(JSON.stringify(value));
}
' "$file" "$key"
}

build_org_header() {
  local title="$1"
  local author="$2"
  local description="$3"
  local published="$4"
  local site="$5"
  local domain="$6"
  local source="$7"

  printf '#+title: %s\n' "$title"
  [[ -n "$author" ]] && printf '#+author: %s\n' "$author"
  [[ -n "$description" ]] && printf '#+description: %s\n' "$description"
  [[ -n "$published" ]] && printf '#+date: %s\n' "$published"
  [[ -n "$site" ]] && printf '#+site: %s\n' "$site"
  [[ -n "$domain" ]] && printf '#+domain: %s\n' "$domain"
  printf '#+source: %s\n' "$source"
  printf '#+filetags: :clipping:\n\n'
}

OUTPUT_MODE="default"
OUTPUT_FILE=""
OUTPUT_DIR=""
SOURCE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --stdout)
      OUTPUT_MODE="stdout"
      shift
      ;;
    --output)
      [[ $# -ge 2 ]] || { echo "--output requires a file path" >&2; exit 1; }
      OUTPUT_MODE="file"
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --output-dir)
      [[ $# -ge 2 ]] || { echo "--output-dir requires a directory path" >&2; exit 1; }
      OUTPUT_MODE="dir"
      OUTPUT_DIR="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
    *)
      if [[ -n "$SOURCE" ]]; then
        echo "Only one source is supported" >&2
        exit 1
      fi
      SOURCE="$1"
      shift
      ;;
  esac
done

if [[ -z "$SOURCE" && $# -gt 0 ]]; then
  SOURCE="$1"
  shift
fi

if [[ -z "$SOURCE" || $# -gt 0 ]]; then
  usage >&2
  exit 1
fi

for cmd in defuddle pandoc node; do
  if ! command_exists "$cmd"; then
    echo "$cmd is not installed or not in PATH" >&2
    exit 1
  fi
done

JSON_TEMP_FILE=$(mktemp)
HTML_TEMP_FILE=$(mktemp)
ORG_TEMP_FILE=$(mktemp)

cleanup() {
  rm -f "$JSON_TEMP_FILE" "$HTML_TEMP_FILE" "$ORG_TEMP_FILE"
}
trap cleanup EXIT

defuddle parse "$SOURCE" --json >"$JSON_TEMP_FILE"

if [[ ! -s "$JSON_TEMP_FILE" ]]; then
  echo "Failed to extract content from $SOURCE" >&2
  exit 1
fi

TITLE=$(json_get "$JSON_TEMP_FILE" "title")
AUTHOR=$(json_get "$JSON_TEMP_FILE" "author")
DESCRIPTION=$(json_get "$JSON_TEMP_FILE" "description")
PUBLISHED=$(json_get "$JSON_TEMP_FILE" "published")
SITE=$(json_get "$JSON_TEMP_FILE" "site")
DOMAIN=$(json_get "$JSON_TEMP_FILE" "domain")
CONTENT=$(json_get "$JSON_TEMP_FILE" "content")

if [[ -z "$TITLE" ]]; then
  TITLE=$(basename "$SOURCE")
fi

if [[ -z "$CONTENT" ]]; then
  echo "Failed to extract article content from $SOURCE" >&2
  exit 1
fi

printf '%s' "$CONTENT" >"$HTML_TEMP_FILE"
pandoc --wrap=none -f html -t org "$HTML_TEMP_FILE" -o "$ORG_TEMP_FILE"

if [[ ! -s "$ORG_TEMP_FILE" ]]; then
  echo "Conversion to Org format failed" >&2
  exit 1
fi

case "$OUTPUT_MODE" in
  stdout)
    build_org_header "$TITLE" "$AUTHOR" "$DESCRIPTION" "$PUBLISHED" "$SITE" "$DOMAIN" "$SOURCE"
    cat "$ORG_TEMP_FILE"
    ;;
  file)
    mkdir -p "$(dirname "$OUTPUT_FILE")"
    {
      build_org_header "$TITLE" "$AUTHOR" "$DESCRIPTION" "$PUBLISHED" "$SITE" "$DOMAIN" "$SOURCE"
      cat "$ORG_TEMP_FILE"
    } >"$OUTPUT_FILE"
    printf 'Content saved to %s\n' "$OUTPUT_FILE" >&2
    ;;
  dir|default)
    if [[ "$OUTPUT_MODE" == "default" ]]; then
      OUTPUT_DIR=$(default_output_dir)
    fi
    mkdir -p "$OUTPUT_DIR"
    TIMESTAMP=$(date +"%Y%m%dT%H%M%S")
    SAFE_TITLE=$(sanitize_filename_component "$TITLE")
    OUTPUT_FILE="${OUTPUT_DIR%/}/${TIMESTAMP}--${SAFE_TITLE}__clipping.org"
    {
      build_org_header "$TITLE" "$AUTHOR" "$DESCRIPTION" "$PUBLISHED" "$SITE" "$DOMAIN" "$SOURCE"
      cat "$ORG_TEMP_FILE"
    } >"$OUTPUT_FILE"
    printf 'Content saved to %s\n' "$OUTPUT_FILE" >&2
    ;;
esac
