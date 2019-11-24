#!/bin/sh

set -e

component=$1

if [ "${component}" = "ucd" ]; then
    version=${2:-"UCD/latest"}
    echo "UCD: ${version}"
    mkdir -p data/ucd/
    cd data/ucd/
    echo "Downloading UCD.zip"
    curl "https://www.unicode.org/Public/${version}/ucd/UCD.zip" > UCD.zip
    echo "Unpacking UCD.zip"
    unzip UCD.zip
elif [ "${component}" = "ucdxml" ]; then
    version=${2:-"UCD/latest"}
    echo "UCDXML: ${version}"
    mkdir -p data/ucdxml/
    cd data/ucdxml/
    echo "Downloading ucd.nounihan.grouped.zip"
    curl "https://www.unicode.org/Public/${version}/ucdxml/ucd.nounihan.grouped.zip" > ucd.nounihan.grouped.zip
elif [ "${component}" = "udhr" ]; then
    echo "UDHR"
    mkdir -p data/udhr/
    cd data/udhr/
    echo "Downloading full_all.txt"
    curl "https://www.unicode.org/udhr/assemblies/full_all.txt" > full_all.txt
else
    echo "Unrecognised component: \"${component}\""
fi
