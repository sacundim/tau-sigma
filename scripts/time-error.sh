#!/usr/bin/env bash
#
# This is one of the parallel subscripts of `examples.sh`.  This
# computes time eror for the given noise data source.
#

set -x -e
cd `dirname $0`/..

TMP=$1
noise=$2

cat ${TMP}/${noise}.csv \
    | tau-sigma chart \
                --label "time error" \
                --out ${TMP}/${noise}_phase.svg

$CONVERT ${TMP}/${noise}_phase.svg images/${noise}_phase.png
