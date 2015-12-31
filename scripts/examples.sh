#!/usr/bin/env bash
#
# Generate some example charts
#

set -xe
cd `dirname $0`/..

for noise in wpm fpm wfm ffm rwfm
do
    tau-sigma noise --${noise} 0.95 \
        | tau-sigma chart --out images/${noise}_phase.svg

    tau-sigma noise --${noise} 0.95 --frequency \
        | tau-sigma chart --out images/${noise}_frequency.svg
    
    tau-sigma noise --${noise} 0.95 \
        | tau-sigma adev --tau0 1 --max-tau 9 \
        | tau-sigma loglog --out images/${noise}_adev.svg
done

