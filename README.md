`tau-sigma`: A Simple Utility for Frequency Stability Analysis
==============================================================

This is a simple command-line utilty for analyzing the frequency
stability of clocks in terms of their
[**Allan deviation**](http://en.wikipedia.org/wiki/Allan_variance).
It has the following subcommands:

1. `adev`: Read a phase error series from standard input and emit
   tau/sigma ADEV pairs to standard output as CSV.  Uses the standard
   overlapped Allan deviation estimator.
2. `loglog`: Generate a tau/sigma log-log chart from the output of
   `adev`.
3. `chart`: Generate a linear chart from time series data.  Meant for
   plotting time or frequency error series.
4. `noise`: A random spectral noise generator for white, flicker and
   random walk frequency noise types, and mixes thereof.
5. `convert`: Conversion between phase/frequency data series and units.

This tool has so far been written primarily with simplicity in mind,
not performance.

Example usages:

    tau-sigma noise --wfm 1.0 --ffm 0.25 --rwfm 0.1 \
        | tau-sigma adev --tau0 86400 \
        | tau-sigma loglog --out mixed_ADEV.svg

    tau-sigma noise --wfm 0.0 --ffm 1.0 --rwfm 0.1 --frequency \
        | tau-sigma chart --out mixed_frequencies.svg


TODO
----

* Unit test!
* Error bars!
* Triple check the flicker noise generation code.
* Use scientific notation for log/log chart Y axis labels.
* Smart scaling of log/log chart axes.  We need same-sized decades in
  both axes.
* Generate charts in other formats than SVG.
* Frequency spectra
* Phase noises
* Other stability statistics


References
----------

* Riley, William and David A. Howe.  2008.
  [*Handbook of Frequency Stability Analysis*](http://tf.nist.gov/general/pdf/2220.pdf).
  National Institute of Standards and Technology Special Publication
  1065, July 2008. Boulder, Colorado: National Institute of Standards
  and Technology.
