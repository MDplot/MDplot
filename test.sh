#!/bin/bash
Rscript MDplot/tests/MDplot_bash.R MDplot_hbond files=MDplot/data/MDplot_hbond_example.tar.gz
acroread Rplots.pdf
