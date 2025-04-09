#!/bin/bash

OUTPUT_FILE="$1"

if [ -z "$OUTPUT_FILE" ]; then
    echo "Usage: $0 <output_file>"
    exit 1
fi

echo "1 0 sync_job_name cd $(pwd) && python3 sync_do.py" > "$OUTPUT_FILE"
echo "SUCCESS: Anacron command written to $OUTPUT_FILE"