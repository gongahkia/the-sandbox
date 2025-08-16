#!/bin/bash

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

if ! command_exists git || ! command_exists curl || ! command_exists jq; then
    echo "Error: This script requires git, curl, and jq to be installed."
    exit 1
fi

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <repository_url> <output_file>"
    exit 1
fi

REPO_URL="$1"
OUTPUT_FILE="$2"

OWNER=$(echo "$REPO_URL" | sed -E 's/.*github\.com\/([^/]+)\/([^/]+).*/\1/')
REPO=$(echo "$REPO_URL" | sed -E 's/.*github\.com\/([^/]+)\/([^/]+).*/\2/')

echo "Flower boy starting up..."
echo "Cloning repository..."
git clone "$REPO_URL" temp_repo
if [ $? -ne 0 ]; then
    echo "Error: Failed to clone the repository."
    exit 1
fi

cd temp_repo || exit 1

echo "Fetching issues..."
ISSUES=$(curl -s "https://api.github.com/repos/$OWNER/$REPO/issues")

if [ "$(echo "$ISSUES" | jq length)" -eq 0 ]; then
    echo "No issues found for this repository."
    cd ..
    rm -rf temp_repo
    exit 0
fi

echo "Writing issues to $OUTPUT_FILE..."
echo "===================================" > "$OUTPUT_FILE"
echo "Issues for $OWNER/$REPO" >> "$OUTPUT_FILE"
echo "===================================" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

echo "$ISSUES" | jq -r '.[] | "Issue #\(.number)\nTitle: \(.title)\nState: \(.state)\nCreated at: \(.created_at)\nBody:\n\(.body)\n-----------------------------------"' >> "$OUTPUT_FILE"

echo "Issues have been written to $OUTPUT_FILE"

cd ..
rm -rf temp_repo

echo "Script completed successfully."
echo "Thank you for using Flower Boy."