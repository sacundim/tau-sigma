#!/usr/bin/env bash
#
# Generate charts for Arnold #36
#

set -xe
cd `dirname $0`/..

TMP=`mktemp -d -t tmp`
CONVERT="convert -density 1200 -resize 800x800"

cat example-data/arnold36.csv \
    | tau-sigma adev --tau0 86400 --max-tau 50 \
    | tau-sigma loglog \
                --label "ADEV" \
                --out ${TMP}/arnold36_adev.svg
$CONVERT ${TMP}/arnold36_adev.svg images/arnold36_adev.png

cat example-data/arnold36.csv \
    | tau-sigma theobr --tau0 86400 --max-tau 100 \
    | tau-sigma loglog \
                --label "TheoBR" \
                --out ${TMP}/arnold36_theobr.svg
$CONVERT ${TMP}/arnold36_theobr.svg images/arnold36_theobr.png
