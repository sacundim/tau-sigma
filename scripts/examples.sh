#!/usr/bin/env bash
#
# Generate some example charts.  This requires:
#
# 1. GNU Parallel;
# 2. ImageMagick with librsvg.
#

set -x -e
cd `dirname $0`/..

TMP=`mktemp -d -t tmp`
TAUS=10

export CONVERT="convert -density 300 -resize 600x600"

parallel "tau-sigma noise --{} 1.0 > ${TMP}/{}.csv" \
         ::: wpm fpm wfm ffm rwfm

parallel "scripts/time-error.sh ${TMP} {}" \
         ::: wpm fpm wfm ffm rwfm

parallel "scripts/frequency-error.sh ${TMP} {}" \
         ::: wpm fpm wfm ffm rwfm

parallel "scripts/one-stat.sh ${TMP} {1} {2} ${TAUS}" \
         ::: wpm fpm wfm ffm rwfm \
         ::: adev mdev tdev hdev totdev

parallel "scripts/one-stat.sh ${TMP} {1} theobr $(($TAUS * 10))" \
         ::: wpm fpm wfm ffm rwfm
