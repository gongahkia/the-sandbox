#!/bin/sh

if [ -f /app/main.go ]; then
  go run /app/main.go
else
  echo "No Go source file found."
fi