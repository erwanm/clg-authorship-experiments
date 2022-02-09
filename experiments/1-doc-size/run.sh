if [ $# -ne 4 ]; then
    echo "args: EXPE1_WORK_DIR SIZE TASKS_DIR NCORES" 1>&2
    exit 1
fi
EXPE1_WORK_DIR="$1"
SIZE="$2"
TASKS_DIR="$3"
NCORES="$4"


prepare-input-data.sh -l english -i "$EXPE1_WORK_DIR/$SIZE/impostors" "$EXPE1_WORK_DIR/$SIZE/data" "$EXPE1_WORK_DIR/$SIZE/process"
task-distrib-daemon.sh  -s 30s -p 1 -v -q 20 "$TASKS_DIR" $NCORES >"$EXPE1_WORK_DIR/$SIZE/task-daemon.log" &
train-top-level.sh -r -P "$TASKS_DIR/mytasks" -o  '-c -s' "$EXPE1_WORK_DIR/$SIZE/process"
