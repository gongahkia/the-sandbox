#!/bin/sh

if [ -f /app/script.php ]; then
  php /app/script.php
else
  echo "No PHP script file found."
fi