#!/bin/bash

# ---------- PRESET DEFINITION ----------

jsonFilePath="sanitised.json"

# ---------- FUNCTION DEFINITION -----------

get_random_url() {
    local urls=("$@")
    local num_urls=${#urls[@]}
    local random_index=$(( RANDOM % num_urls ))
    local random_url="${urls[random_index]}"
    echo "$random_url"
}

# ---------- DOWNLOAD PHASE ----------

# extract gif URLs from all arrays
urls=($(jq -r '.tenor[]' "$jsonFilePath"))
urls+=($(jq -r '.giphy[]' "$jsonFilePath"))
urls+=($(jq -r '.gifb[]' "$jsonFilePath"))

# shuffle URLS
shuffled_urls=($(shuf -e "${urls[@]}"))

# download relevant gifs
mkdir httpCatsDownload
for (( i=1; i<6; i++ )); do
    chosenURL=$(get_random_url "${shuffled_urls[@]}")
    curl -o httpCatsDownload/gif$i.gif $chosenURL
done
echo "http cat has downloaded your GIFs"