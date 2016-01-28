#!/usr/bin/env bash
#
# Script to generate an ADEV graph for the Earth's rotation, using
# IERS length-of-day data.  This requires ImageMagick with librsvg,
# and the `xsv` utilty:
#
# https://github.com/BurntSushi/xsv
#

set -x -e
cd `dirname $0`/..

INPUT=example-data/eopc04_08.1962-01-01_2015-12-15.csv

TMP=`mktemp -d -t tmp`
CONVERT="convert -density 300 -resize 800x800"

xsv select -d ';' LOD ${INPUT} \
    | xsv fmt -d ';' \
    | tail -n '+2' \
    | tau-sigma convert \
                --input-domain Frequency \
                --input-denominator 86400 \
                --output-domain Phase \
                --output-denominator 86400 \
    | tau-sigma totdev --tau0 86400 --max-tau 8.64e8 \
    | tau-sigma loglog \
                --label "totdev" \
                --xunit 86400 --out ${TMP}/earth-1960-2015.svg
$CONVERT ${TMP}/earth-1960-2015.svg images/earth-1960-2015.png
