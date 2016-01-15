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

for noise in wpm fpm wfm ffm rwfm
do
    tau-sigma noise --${noise} ${!noise} > ${TMP}/${noise}.csv

    cat ${TMP}/${noise}.csv \
        | tau-sigma chart \
                    --label "time error" \
                    --out images/${noise}_phase.svg

    cat ${TMP}/${noise}.csv \
        | tau-sigma convert \
                    --input-domain Phase \
                    --input-denominator 1 \
                    --output-domain Frequency \
                    --output-denominator 1 \
        | tau-sigma chart \
                    --label "frequency error" \
                    --out images/${noise}_frequency.svg

    for stat in adev mdev tdev hdev totdev
    do
        cat ${TMP}/${noise}.csv \
            | tau-sigma ${stat} --tau0 1 --max-tau ${TAUS} \
            | tau-sigma loglog \
                        --label "${stat}" \
                        --out images/${noise}_${stat}.svg
    done
    
    cat ${TMP}/${noise}.csv \
        | tau-sigma theobr --tau0 1 --max-tau $(($TAUS * 10)) \
        | tau-sigma loglog \
                    --label "TheoBR" \
                    --out images/${noise}_theobr.svg
done

