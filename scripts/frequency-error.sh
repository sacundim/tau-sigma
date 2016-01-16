#!/usr/bin/env bash
#
# This is one of the parallel subscripts of `examples.sh`.  This
# computes frequency eror for the given noise data source.
#

set -x -e
cd `dirname $0`/..

TMP=$1
noise=$2

cat ${TMP}/${noise}.csv \
    | tau-sigma convert \
                --input-domain Phase \
                --input-denominator 1 \
                --output-domain Frequency \
                --output-denominator 1 \
    | tau-sigma chart \
                --label "frequency error" \
                --out ${TMP}/${noise}_frequency.svg

$CONVERT ${TMP}/${noise}_frequency.svg images/${noise}_frequency.png

