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

# Noise levels for each type of noise, to produce a chart with the
# correct range to fit the chart and produce the canonical slope.
wpm=0.525
fpm=0.41
wfm=0.575
ffm=0.4
rwfm=0.4

export CONVERT="convert -density 1200 -resize 600x600"

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
