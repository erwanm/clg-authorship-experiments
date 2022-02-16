
source common-lib.sh
source file-lib.sh
source pan-utils.sh

progName=$(basename "$BASH_SOURCE")

sleepTime=30s

modelTypes="meta basic GI univ"

cleanupLinks=""
testMetaOpts=""

if [ $# -ne 1 ] && [ $# -ne 3 ]; then
    echo "args: EXPE_WORK_DIR [NCORES TASKS-DIR]" 1>&2
    exit 1
fi
EXPE_WORK_DIR="$1"
NCORES="$2"
TASKS_DIR="$3"


# this is in case the experiment was run in a /tmp directory instead of its current location
# the paths would contain /tmp/<path>
# the function creates some symlinks in /tmp (or wherever) to prevent all the 'file not found' errors
# nothing is deleted but this creates symlinks which may not be deleted if the script doesn't terminate normally
# also it may not work if the symlinks or other dir already exist at the same location
function trickDifferentExpeDir {
    local processDir="$1"

    targetDir=$(dirname "$processDir")
    name=$(basename "$targetDir")
    prefix=$(head -n 1 "$processDir/selected-meta-prefixes.list" | cut -f 1 | sed s#//*#/#g)
#    echo "DEBUG prefix=$prefix" 1>&2
    dir=${prefix%/process/selected-meta-configs*}
    if [ ! -d "$dir" ]; then
	cleanupLinks=""
	dir0=$(dirname "$dir")
	if [ ! -d "$dir0" ]; then
	    mkdir "$dir0" 
	fi
	linkAbsolutePath "$dir0" "$targetDir"
	if [ $? -eq 0 ]; then
	    cleanupLinks="$cleanupLinks $dir0/$name"
	fi
#	echo "  DEBUG creating $name in $dir0" 1>&2
	if [ ! -d "/tmp/$name" ]; then
	    linkAbsolutePath "/tmp" "$targetDir"
#	    echo "  DEBUG creating $name in /tmp" 1>&2
	    if [ $? -eq 0 ]; then
		cleanupLinks="$cleanupLinks /tmp/$name"
	    fi
	fi
    fi
    
}


function getModelTypeFromPrefix {
    local prefix="$1"
    n=$(echo "$prefix" | grep "strategy-as-meta" 2>/dev/null | wc -l)
    if [ $n -eq 1 ]; then
	tmp0=${prefix#*strategy-as-meta.}
	echo ${tmp0%.*}
    else
	echo "meta"
    fi
}


function extractSelectedModel {
    local processDir="$1"
    local modelType="$2"

    bestFile="$processDir/selected-meta-prefixes.list"
    if [ $modelType == "selected" ]; then
	prefix=$(head -n 1 "$bestFile" | cut -f 1)
    else
	if [  $modelType == "meta" ]; then
	    prefix=$(cat "$bestFile" | cut -f 1 | grep -v "strategy-as-meta" | head -n 1)
	else
	    prefix=$(cat "$bestFile" | cut -f 1 | grep  "strategy-as-meta.$modelType" | head -n 1)
	fi
    fi
    if [ -z "$prefix" ]; then # no best for this type in the top 100
	echo
    else
	echo "$prefix"
    fi
}


function computeSeenAuthorCases {
    local datasetDir="$1"
    local resultsDir="$2"

    fullFile="$datasetDir/full-dataset.tsv"
    if [ -f "$fullFile" ]; then
	colTrainTest=$(head -n 1 "$fullFile" | tr '\t' '\n' | grep -n "train.or.test" | cut -d ':' -f 1)
	colSeen=$(head -n 1 "$fullFile" | tr '\t' '\n' | grep -n "author.seen" | cut -d ':' -f 1)
	colCase=$(head -n 1 "$fullFile" | tr '\t' '\n' | grep -n "case.id" | cut -d ':' -f 1)
	colAnswer=$(head -n 1 "$fullFile" | tr '\t' '\n' | grep -n "answer" | cut -d ':' -f 1)
	tail -n +2 "$fullFile"  | filter-column.pl -v test 1 "$colTrainTest" | filter-column.pl -v TRUE 1 "$colSeen" | cut -f "$colCase,$colAnswer" >"$resultsDir"/seen.cases
	tail -n +2 "$fullFile"  | filter-column.pl -v test 1 "$colTrainTest" | filter-column.pl -v -n TRUE 1 "$colSeen" | cut -f "$colCase,$colAnswer" >"$resultsDir"/unseen.cases
    else
	echo "Warning: file '$datasetDir/full-dataset.tsv' not found, cannot evaluate by seen/unseen author" 1>&2
    fi
}


function evalSeenUnseen {
    local outputDir="$1"
    local seenUnseen="$2"
    local seenUnseenDir="$3"

    cat "$outputDir"/*.answers | filter-column.pl "$seenUnseenDir/$seenUnseen.cases" 1 1 >"$outputDir/$seenUnseen.answers"
    n1=$(cat "$outputDir/$seenUnseen.answers" | wc -l)
    n2=$(cat "$seenUnseenDir/$seenUnseen.cases" | wc -l)
    if [ $n1 -ne $n2 ]; then
	echo "Error: different number of lines between '$outputDir/$seenUnseen.answers' and '$seenUnseenDir/$seenUnseen.cases'" 1>&2
	exit 17
    fi
    scoreAUC=$(auc.pl -p 6  "$seenUnseenDir/$seenUnseen.cases" "$outputDir/$seenUnseen.answers")
    scoreC1=$(accuracy.pl -c -p 6  "$seenUnseenDir/$seenUnseen.cases" "$outputDir/$seenUnseen.answers" | cut -f 1)
    scoreFinal=$(perl -e "printf(\"%.6f\n\", $scoreAUC * $scoreC1);")
    if [ -z "$scoreAUC" ] || [ -z "$scoreC1" ] || [ -z "$scoreFinal" ] ; then
        echo "$progName error: was not able to extract one of the evaluation scores in $outputDir" 1>&2
        exit 1
    fi
    echo -e "$scoreFinal\t$scoreAUC\t$scoreC1"


} 



function runEval {
    local prefix="$1"
    local dataset="$2"
    local inputDir="$3"
    local outputDir="$4"
    local seenUnseenDir="$5"
    local size="$6"
    local thisModelSelected="$7"
    local modelType="$8"

    comm="test-meta-model.sh -p $testMetaOpts \"$prefix\" \"$dataset\" \"$inputDir\" \"$outputDir\" >\"$outputDir/test-meta-model.out\" 2>\"$outputDir/test-meta-model.err\""
    eval "$comm"
    if [ $? -ne 0 ]; then
	exit 11
    fi
    n=$(ls "$outputDir"/*.perf 2>/dev/null | wc -l)
    if [ $n -ne 1 ]; then
	echo "Error: no .perf file found in '$outputDir'" 1>&2
	exit 1
    fi
    perf=$(cat "$outputDir"/*.perf)
    echo -e "$size\t$thisModelSelected\t$modelType\t$trainTest\t$perf" >"$outputDir/results.tsv"

    trainOrTest=$(basename "${dataset%.tsv}")
    if [ "$trainOrTest" == "test" ] && [ -f "$seenUnseenDir/seen.cases" ] && [ -f "$seenUnseenDir/unseen.cases" ]; then
	n=$(ls "$outputDir"/*.answers 2>/dev/null | wc -l)
	if [ $n -ne 1 ]; then
	    echo "Warning: no .answers file in $thisOutputDir" 1>&2
	else
	    seenPerf=$(evalSeenUnseen "$outputDir" "seen" "$seenUnseenDir")
	    echo -e "$size\t$thisModelSelected\t$modelType\ttest.seen\t$seenPerf" >>"$outputDir/results.tsv"
	    unseenPerf=$(evalSeenUnseen "$outputDir" "unseen" "$seenUnseenDir")
	    echo -e "$size\t$thisModelSelected\t$modelType\ttest.unseen\t$unseenPerf" >>"$outputDir/results.tsv"
	fi
    fi    

}



outputDir="$EXPE_WORK_DIR/results"
[ -d "$outputDir" ] || mkdir "$outputDir"


if [ ! -z "$NCORES" ]; then
    mkdirSafe "$TASKS_DIR"
    n=$(ls "$TASKS_DIR" 2>/dev/null | wc -l)
    if [ $n -ne 0 ]; then
	echo "Tasks dir $TASKS_DIR not empty, aborting" 1>&2
	exit 15
    fi
    task-distrib-daemon.sh  -s 20s -p 2 -q 30  "$TASKS_DIR" "$NCORES" >"$outputDir"/task-daemon.log &
    waitFile=$(mktemp --tmpdir "$progName.wait.XXXXXXXXX")
    testMetaOpts="$testMetaOpts -P \"$TASKS_DIR/evaltask\""
fi


for sizeDir in "$EXPE_WORK_DIR"/*; do
    inputDir="$sizeDir/process"
    if [ -d "$sizeDir" ] && [ -d "$inputDir" ]; then
	size=$(basename "$sizeDir")
	sizeOutputDir="$outputDir/$size"
	mkdirSafe "$sizeOutputDir"
	if [ -f "$EXPE_WORK_DIR/full-dataset.tsv" ] && [ -f "$EXPE_WORK_DIR/train.tsv" ] && [ -f "$EXPE_WORK_DIR/test.tsv" ]; then
	    datasetsDir="$EXPE_WORK_DIR"
	else
	    if [ -f "$sizeDir/full-dataset.tsv" ] &&[ -f "$sizeDir/train.tsv" ] && [ -f "$sizeDir/test.tsv" ]; then
		datasetsDir="$sizeDir"
	    else
		echo "Error: full-dataset.tsv and/or train.tsv and/or test.tsv not found in $EXPE_WORK_DIR nor $sizeDir" 1>&2
		exit 2
	    fi
	fi
	computeSeenAuthorCases "$datasetsDir" "$sizeOutputDir"
	if  [ -f "$inputDir/best.results" ] && [ -d "$inputDir/selected-meta-configs" ]; then
	    trickDifferentExpeDir "$inputDir"
	    selectedPrefix=$(extractSelectedModel "$inputDir" "selected")
	    selectedModelType=$(getModelTypeFromPrefix "$selectedPrefix")
	    for modelType in $modelTypes; do
		modelPrefix=$(extractSelectedModel "$inputDir" "$modelType")
		if [ ! -z "$modelPrefix" ]; then
		    mkdirSafe "$sizeOutputDir/$modelType"
		    echo "$modelPrefix" >"$sizeOutputDir/$modelType"/model.prefix
		    thisModelSelected="FALSE"
		    if [ "$modelType" == "$selectedModelType" ]; then
			if [ "$selectedPrefix" != "$modelPrefix" ]; then # sanity check
			    echo "Error: sanity check failed, $modelType has prefix $modelPrefix but selected has prefix $selectedPrefix" 1>&2
			    exit 3
			fi
			thisModelSelected="TRUE"
		    fi
		    for trainTest in train test; do
			thisOutputDir="$sizeOutputDir/$modelType/$trainTest"
			n=$(ls "$thisOutputDir/results.tsv" 2>/dev/null | wc -l)
			if [ $n -eq 1 ]; then
			    echo "$thisOutputDir: already done, skipping." 1>&2
			else
			    rm -rf "$thisOutputDir"
			    mkdir "$thisOutputDir"
			    if [ -z "$NCORES" ]; then
				runEval "$modelPrefix" "$datasetsDir/$trainTest.tsv" "$inputDir" "$thisOutputDir" "$sizeOutputDir" "$size" "$thisModelSelected" "$modelType"
				if [ $? -ne 0 ]; then
				    exit 11
				fi
			    else
				runEval "$modelPrefix" "$datasetsDir/$trainTest.tsv" "$inputDir" "$thisOutputDir" "$sizeOutputDir" "$size" "$thisModelSelected" "$modelType" &
				echo "$thisOutputDir/results.tsv" >> "$waitFile"
			    fi
			fi
		    done
		else
		    for trainTest in train test; do
			thisOutputDir="$sizeOutputDir/$modelType/$trainTest"
			mkdirSafe "$thisOutputDir"
			echo -e "$size\tFALSE\t$modelType\t$trainTest\tNA" >"$thisOutputDir/results.tsv"
			if [ "$trainTest" == "test" ]; then
			    echo -e "$size\tFALSE\t$modelType\ttest.seen\tNA" >>"$thisOutputDir/results.tsv"
			    echo -e "$size\tFALSE\t$modelType\ttest.unseen\tNA" >>"$thisOutputDir/results.tsv"
			fi
		    done
		fi
	    done
	else
	    echo "$sizeDir: skipping, no final data." 1>&2
	fi
    fi
done


if [ ! -z "$NCORES" ]; then
    waitFilesList "$progName: evaluating the models." "$waitFile" $sleepTime
    rm -f "$waitFile"
fi

if [ ! -z "$cleanupLinks" ]; then
    rm -f "$cleanupLinks"
fi
echo -e "variable\tselected.by.training\tmodel.type\tevaluated.on\tperf.final\tperf.auc\tperf.accu" >"$outputDir/results.tsv"
cat "$outputDir"/*/*/*/results.tsv >>"$outputDir/results.tsv"
echo "$progName: done."
