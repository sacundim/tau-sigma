# `tau-sigma`: A Simple Utility for Frequency Stability Analysis

[![Build Status](https://travis-ci.org/sacundim/tau-sigma.svg?branch=master)](https://travis-ci.org/sacundim/tau-sigma)

This is a simple command-line utilty for analyzing the frequency
stability of clocks in terms of their
[**Allan deviation**](http://en.wikipedia.org/wiki/Allan_variance).
It has the following subcommands:

1. `adev`: Read a phase error series from standard input and emit
   tau/sigma ADEV pairs to standard output as CSV.  Uses the standard
   overlapped Allan deviation estimator.
2. `theobr`: Like `adev` but uses TheoBR, a statistic for that
   produces better estimates of long-term stability (but is much
   slower than ADEV).
3. `loglog`: Generate a tau/sigma log-log chart from the output of
   `adev` or `theobr`.
4. `chart`: Generate a linear chart from time series data.  Meant for
   plotting time or frequency error series.
5. `noise`: A random spectral noise generator for white, flicker and
   random walk frequency noise types, and mixes thereof.
6. `convert`: Conversion between phase/frequency data series and units.

This tool has so far been written primarily with simplicity in mind,
not performance.


## Examples

These examples are scripted in [`script/examples.sh`](script/examples.sh).


### Time Domain

* [White phase modulation](images/wpm_phase.svg)
* [Flicker phase modulation](images/fpm_phase.svg)
* [White frequency modulation](images/wfm_phase.svg)
* [Flicker frequency modulation](images/ffm_phase.svg)
* [Random walk frequency modulation](images/rwfm_frequency.svg)


### Frequency Domain

* [White phase modulation](images/wpm_frequency.svg)
* [Flicker phase modulation](images/fpm_frequency.svg)
* [White frequency modulation](images/wfm_frequency.svg)
* [Flicker frequency modulation](images/ffm_frequency.svg)
* [Random walk frequency modulation](images/rwfm_frequency.svg)


### Allan deviation

* [White phase modulation](images/wpm_adev.svg)
* [White frequency modulation](images/wfm_adev.svg)
* [Flicker frequency modulation](images/ffm_adev.svg)
* [Random walk frequency modulation](images/rwfm_adev.svg)

### Arnold #36 pocket chronometer

I wrote this utilty to help me in my quest to understand the
performance of historical precision timepieces.  Here's a fine
example: the 1779/80 Greenwich trial of
[John Arnold's pocket chronometer #36](http://collections.rmg.co.uk/collections/objects/207131.html),
one of the very earliest successful precision watches.  Read more in
[`example-data/README.md`](example-data/README.md).

* [Arnold #36 TheoBR](images/arnold36_theobr.svg)


## TODO

* Error bars!
* Triple check the flicker noise generation code.
* Smart scaling of log/log chart axes.  We need same-sized decades in
  both axes.
* Generate charts in other formats than SVG.
* Frequency spectra
* Other stability statistics


## References

* Howe, D.A. and T.N. Tasset.  2004.
  ["Theo1: Characterization of very long-term frequency stability."](http://tf.nist.gov/timefreq/general/pdf/1990.pdf)
  *Proceedings of the 18th European Frequency and Time Forum (2004)*.
* Riley, William and David A. Howe.  2008.
  [*Handbook of Frequency Stability Analysis*](http://tf.nist.gov/general/pdf/2220.pdf).
  National Institute of Standards and Technology Special Publication
  1065, July 2008. Boulder, Colorado: National Institute of Standards
  and Technology.
