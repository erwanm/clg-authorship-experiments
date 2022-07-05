
source common-lib.sh
source file-lib.sh
source pan-utils.sh

progName=$(basename "$BASH_SOURCE")

SCORE_COL_NO=4


if [ $# -ne 2 ]; then
    echo "args: MAIN_RESULT_DIR PAN21_DIR" 1>&2
    echo "      MAIN_RESULT_DIR contains dir 'results' with tested systems. PAN21_DIR contains predicted answers" 1>&2
    exit 1
fi
EXPE_WORK_DIR="$1"
PAN21_DIR="$2"



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
    if [ $n1 -gt 0 ]; then
	scoreAUC=$(auc.pl -p 6  "$seenUnseenDir/$seenUnseen.cases" "$outputDir/$seenUnseen.answers")
	scoreC1=$(accuracy.pl -c -p 6  "$seenUnseenDir/$seenUnseen.cases" "$outputDir/$seenUnseen.answers" | cut -f 1)
	scoreFinal=$(perl -e "printf(\"%.6f\n\", $scoreAUC * $scoreC1);")
	if [ -z "$scoreAUC" ] || [ -z "$scoreC1" ] || [ -z "$scoreFinal" ] ; then
            echo "$progName error: was not able to extract one of the evaluation scores in $outputDir" 1>&2
            exit 1
	fi
	echo -e "$scoreFinal\t$scoreAUC\t$scoreC1"
    else # no seen/unseen author at all
	echo -e "NA\t\NA\tNA"
    fi


} 





function runEval {
    local trainOrTest="$1"
    local inputDir="$2"
    local outputDir="$3"
    local seenUnseenDir="$4"
    local size="$5"
    local thisModelSelected="$6"
    local modelType="$7"
    local goldFile="$8"

    f="$inputDir/${size}.predicted.${trainOrTest}.${modelType#*.}.tsv"
    if [ ! -s "$f" ]; then
	echo "Error: '$f' does not exist" 1>&2
	exit 1
    fi
    cut -f $SCORE_COL_NO "$f" > "$outputDir"/col_answers.tmp
    cut -f 1 "$goldFile" | paste - "$outputDir"/col_answers.tmp > "$outputDir/$modelType.answers"
    rm -f "$outputDir"/col_answers.tmp
    scoreAUC=$(auc.pl -p 6  "$goldFile" "$outputDir/$modelType.answers")
    scoreC1=$(accuracy.pl -c -p 6  "$goldFile" "$outputDir/$modelType.answers" | cut -f 1)
    scoreFinal=$(perl -e "printf(\"%.6f\n\", $scoreAUC * $scoreC1);")
    if [ -z "$scoreAUC" ] || [ -z "$scoreC1" ] || [ -z "$scoreFinal" ] ; then
        echo "$progName error: was not able to extract one of the evaluation scores in $outputDir" 1>&2
        exit 1
    fi
    echo -e "$scoreFinal\t$scoreAUC\t$scoreC1" > "$outputDir/$modelType.perf"

   perf=$(cat "$outputDir/$modelType.perf")
    echo -e "$size\t$thisModelSelected\t$modelType\t$trainTest\t$perf" >"$outputDir/results.tsv"
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
if [ ! -d "$outputDir" ] ; then
    echo "ERROR" 1>&2
fi


for sizeDir in "$outputDir"/*; do
    if [ -d "$sizeDir" ]; then
	size=$(basename "$sizeDir")
	sizeOutputDir="$sizeDir"
	if [ -f "$EXPE_WORK_DIR/full-dataset.tsv" ] && [ -f "$EXPE_WORK_DIR/train.tsv" ] && [ -f "$EXPE_WORK_DIR/test.tsv" ]; then
	    datasetsDir="$EXPE_WORK_DIR"
	else
	    if [ -f "$EXPE_WORK_DIR/$size/full-dataset.tsv" ] &&[ -f "$EXPE_WORK_DIR/$size/train.tsv" ] && [ -f "$EXPE_WORK_DIR/$size/test.tsv" ]; then
		datasetsDir="$EXPE_WORK_DIR/$size"
	    else
		echo "Error: full-dataset.tsv and/or train.tsv and/or test.tsv not found in $EXPE_WORK_DIR nor $EXPE_WORK_DIR/$size" 1>&2
		exit 2
	    fi
	fi
	if [ ! -s "$sizeDir/seen.cases" ] || [ ! -s "$sizeDir/unseen.cases" ]; then
	    computeSeenAuthorCases "$datasetsDir" "$sizeOutputDir"
	fi
	for modelType in PAN21.regular PAN21.special; do
	    [ -d "$sizeOutputDir/$modelType" ] || mkdir "$sizeOutputDir/$modelType"
	    for trainTest in train test; do
		thisOutputDir="$sizeOutputDir/$modelType/$trainTest"
		rm -rf "$thisOutputDir"
		mkdir "$thisOutputDir"
		runEval "$trainTest" "$PAN21_DIR" "$thisOutputDir" "$sizeDir" "$size" FALSE "$modelType" "$datasetsDir/${trainTest}.tsv"
	    done
	done
    else
	echo "$sizeDir: skipping, no final data." 1>&2
    fi
done


echo -e "variable\tselected.by.training\tmodel.type\tevaluated.on\tperf.final\tperf.auc\tperf.accu" >"$outputDir/results.tsv"
cat "$outputDir"/*/*/*/results.tsv >>"$outputDir/results.tsv"
echo "$progName: done."

