#!/bin/bash

# EM Feb 2022

minSize=""
removeEmptyLines=""

function usage {
  echo
  echo "Usage: ls <input file> | $progName [options] <size> <output dir>"
  echo
  echo "  Reads a set of text files from STDIN and generates an extract of"
  echo "  size <size> (in lines) for every file in <output dir>."
  echo
  echo "  - Requires CLGTextTools active"
  echo "  - If <size> is 0 then the full documents are copied."
  echo "  - Caution: the basename is used for every file, so there must not"
  echo "             be the same filename in different directories."
  echo
  echo "  Options:"
  echo "    -h this help"
  echo "    -e <N>: exclude files with less than N lines."
  echo "    -r remove empty lines before selecting a snippet."
  echo
}






OPTIND=1
while getopts 'he:r' option ; do 
    case $option in
	"h" ) usage
 	      exit 0;;
	"e" ) minSize="$OPTARG";;
	"r" ) removeEmptyLines="1";;
	"?" ) 
	    echo "Error, unknow option." 1>&2
            printHelp=1;;
    esac
done
shift $(($OPTIND - 1))
if [ $# -ne 2 ]; then
    echo "Error: expecting 2 args." 1>&2
    printHelp=1
fi
if [ ! -z "$printHelp" ]; then
    usage 1>&2
    exit 1
fi
size="$1"
outputDir="$2"

[ -d "$outputDir" ] || mkdir "$outputDir"
while read inputFile; do
    if [ ! -f "$inputFile" ]; then
	echo "Warning: file $inputFile not found, ignoring."  1>&2
    else
	f="$inputFile"
	if [ ! -z "$removeEmptyLines" ]; then
	    f=$(mktemp --tmpdir "tmp.$(basename "$0").XXXXXXXX")
	    cat "$inputFile" | grep . >"$f"
	fi
	ok=""
	if [ ! -z "$minSize" ]; then
	    n=$(cat "$f" | wc -l)
	    if [ $n -lt $minSize ]; then
		ok="nope"
	    fi
	fi
	if [ -z "$ok" ]; then
	    if [ $size -eq 0 ]; then
		cat "$f" >"$outputDir"/$(basename "$inputFile")
	    else
		extract-continuous-sample.pl "$f" "$size" >"$outputDir"/$(basename "$inputFile")
	    fi
	fi
	if [ ! -z "$removeEmptyLines" ]; then
	    rm -f "$f"
	fi
    fi 
done
