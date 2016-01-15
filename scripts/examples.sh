#!/usr/bin/env bash
#
# Generate some example charts
#

set -xe
cd `dirname $0`/..

TAUS=9

# Noise levels for each type of noise, to produce a chart with the
# correct range to fit the chart and produce the canonical slope.
wpm=0.57
fpm=0.4
wfm=0.575
ffm=0.4
rwfm=0.4

for noise in wpm fpm wfm ffm rwfm
do
    tau-sigma noise --${noise} ${!noise} \
        | tau-sigma chart \
                    --label "time error" \
                    --out images/${noise}_phase.svg

    tau-sigma noise --${noise} ${!noise} --frequency \
        | tau-sigma chart \
                    --label "frequency error" \
                    --out images/${noise}_frequency.svg
    
    tau-sigma noise --${noise} ${!noise} \
        | tau-sigma adev --tau0 1 --max-tau ${TAUS} \
        | tau-sigma loglog \
                    --label "ADEV" \
                    --out images/${noise}_adev.svg
done

