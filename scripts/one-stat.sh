#!/usr/bin/env bash
#
# This is one of the parallel subscripts of `examples.sh`.  This
# computes the given statistic for the given noise data source.
#

set -x -e
cd `dirname $0`/..

TMP=$1
noise=$2
stat=$3
taus=$4

cat ${TMP}/${noise}.csv \
    | tau-sigma ${stat} --tau0 1 --max-tau ${taus} \
    | tau-sigma loglog \
                --label "${stat}" \
                --out ${TMP}/${noise}_${stat}.svg

$CONVERT ${TMP}/${noise}_${stat}.svg images/${noise}_${stat}.png
