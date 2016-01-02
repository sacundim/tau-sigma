#!/usr/bin/env bash
#
# Generate some example charts
#

set -xe
cd `dirname $0`/..

VALUE=0.575
TAUS=9

for noise in wpm fpm wfm ffm rwfm
do
    tau-sigma noise --${noise} ${VALUE} \
        | tau-sigma chart \
                    --label "time error" \
                    --out images/${noise}_phase.svg

    tau-sigma noise --${noise} ${VALUE} --frequency \
        | tau-sigma chart \
                    --label "frequency error" \
                    --out images/${noise}_frequency.svg
    
    tau-sigma noise --${noise} ${VALUE} \
        | tau-sigma adev --tau0 1 --max-tau ${TAUS} \
        | tau-sigma loglog \
                    --label "ADEV" \
                    --out images/${noise}_adev.svg
done

