
source common-lib.sh
source file-lib.sh

if [ $# -ne 1 ]; then
    echo "args: EXPE1_WORK_DIR" 1>&2
    exit 1
fi
EXPE1_WORK_DIR="$1"


outputDir="$EXPE1_WORK_DIR/results"
[ -d "$outputDir" ] || mkdir "$outputDir"
[ -d "$outputDir/train" ] || mkdir "$outputDir/train"
[ -d "$outputDir/test" ] || mkdir "$outputDir/test"

rm -f "$outputDir/results.tsv"
for sizeDir in "$EXPE1_WORK_DIR"/*; do
    inputDir="$sizeDir/process"
    if [ -d "$sizeDir" ] && [ -d "$inputDir" ]; then
	size=$(basename "$sizeDir")
	if [ ! -d "/tmp/$size" ]; then
	    linkAbsolutePath "/tmp" "$sizeDir"
	fi
	if  [ -f "$inputDir/best.results" ] && [ -d "$inputDir/selected-meta-configs" ]; then
	    metaPrefix=$(head -n 1 "$inputDir/best.results" | cut -f 1)
	    for trainTest in train test; do
		n=$(ls "$outputDir/$trainTest/$size"/*.perf 2>/dev/null | wc -l)
		if [ $n -ne 1 ]; then
		    rm -rf "$outputDir/$trainTest/$size"
		    test-meta-model.sh -p "$metaPrefix" "$EXPE1_WORK_DIR/$trainTest.tsv" "$inputDir" "$outputDir/$trainTest/$size"
		else
		    echo "$outputDir/$trainTest/$size: already done, skipping." 1>&2
		fi
		n=$(ls "$outputDir/$trainTest/$size"/*.perf 2>/dev/null | wc -l)
		if [ $n -ne 1 ]; then
		    echo "Error: no .perf file found in '$outputDir/$trainTest/$size'" 1>&2
		    exit 1
		fi
		echo -en "$size\t$trainTest\t" >>"$outputDir/results.tsv"
		cat "$outputDir/$trainTest/$size"/*.perf  >>"$outputDir/results.tsv"
	    done
	else
	    echo "$sizeDir: skipping, no final data." 1>&2
	fi
    fi
done
