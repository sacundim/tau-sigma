#!/usr/bin/env bash
#
# Generate charts for Arnold #36.  This requires ImageMagick with librsvg.
#

set -xe
cd `dirname $0`/..

TMP=`mktemp -d -t tmp`
CONVERT="convert -density 1200 -resize 800x800"

cat example-data/arnold36.csv \
    | tau-sigma chart \
                --label "Time error" \
                --out ${TMP}/arnold36_phase.svg
$CONVERT ${TMP}/arnold36_phase.svg images/arnold36_phase.png


cat example-data/arnold36.csv \
    | tau-sigma convert \
                --input-domain Phase \
                --input-denominator 86400 \
                --output-domain Frequency \
                --output-denominator 86400 \
    | tau-sigma chart \
                --label "Daily rates" \
                --out ${TMP}/arnold36_frequency.svg
$CONVERT ${TMP}/arnold36_frequency.svg images/arnold36_frequency.png


cat example-data/arnold36.csv \
    | tau-sigma theoh --tau0 86400 --max-tau 8.64e6 \
    | tau-sigma loglog \
                --label "TheoH" \
                --xunit 86400 \
                --out ${TMP}/arnold36_theoh.svg
$CONVERT ${TMP}/arnold36_theoh.svg images/arnold36_theoh.png
