#!/bin/bash
#############################################################################
# Parameters
RKT=/usr/local/racket/bin/

## Default jobs: compute number of cores, divide by 2
if hash nproc 2>/dev/null; then
  CORES=$(nproc)
elif hash sysctl 2>/dev/null; then
  CORES=$(sysctl -n hw.physicalcpu)
else
  CORES=2 # So we get 1 job
fi
JOBS=1

TARGET=${1%/}
LOG=$TARGET.log
NUMITERS=${NUMITERS:-1}

###############################################################################
## Main script
echo "### Running benchmarks for '"$TARGET"'"
if test $NUMITERS; then
 echo "### ("$NUMITERS" iterations per config.)"
fi
$RKT/raco pkg update tools/benchmark-util
$RKT/racket tools/setup-benchmark.rkt $TARGET
$RKT/racket tools/run.rkt -n -i $NUMITERS -j $JOBS -r $RKT $TARGET | tee $LOG
echo "### Saved logfile to '"$LOG"'"
