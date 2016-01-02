#!/usr/bin/env bash
#
# Generate charts for Arnold #36
#

set -xe
cd `dirname $0`/..

cat example-data/arnold36.csv \
    | tau-sigma adev --tau0 86400 --max-tau 50 \
    | tau-sigma loglog \
                --label "ADEV" \
                --out images/arnold36_adev.svg

cat example-data/arnold36.csv \
    | tau-sigma theobr --tau0 86400 --max-tau 150 \
    | tau-sigma loglog \
                --label "TheoBR" \
                --out images/arnold36_theobr.svg
