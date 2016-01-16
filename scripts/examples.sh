#!/usr/bin/env bash
#
# Generate some example charts
#

set -x -e
cd `dirname $0`/..

TMP=`mktemp -d -t tmp`
TAUS=9

# Noise levels for each type of noise, to produce a chart with the
# correct range to fit the chart and produce the canonical slope.
wpm=0.525
fpm=0.41
wfm=0.575
ffm=0.4
rwfm=0.4

CONVERT="convert -density 1200 -resize 600x600"

for noise in wpm fpm wfm ffm rwfm
do
    tau-sigma noise --${noise} ${!noise} > ${TMP}/${noise}.csv

    cat ${TMP}/${noise}.csv \
        | tau-sigma chart \
                    --label "time error" \
                    --out ${TMP}/${noise}_phase.svg
    $CONVERT ${TMP}/${noise}_phase.svg images/${noise}_phase.png

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

    for stat in adev mdev tdev hdev totdev
    do
        cat ${TMP}/${noise}.csv \
            | tau-sigma ${stat} --tau0 1 --max-tau ${TAUS} \
            | tau-sigma loglog \
                        --label "${stat}" \
                        --out ${TMP}/${noise}_${stat}.svg
        $CONVERT ${TMP}/${noise}_${stat}.svg images/${noise}_${stat}.png
    done
    
    cat ${TMP}/${noise}.csv \
        | tau-sigma theobr --tau0 1 --max-tau $(($TAUS * 10)) \
        | tau-sigma loglog \
                    --label "TheoBR" \
                    --out ${TMP}/${noise}_theobr.svg
    $CONVERT ${TMP}/${noise}_theobr.svg images/${noise}_theobr.png
done

