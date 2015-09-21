#!/bin/bash

# pdf
Rscript MDplot/tests/MDplot_bash.R MDplot_clusters files=MDplot/data/MDplot_clusters_example.txt title="cluster try" size=1200,900 outformat=pdf outfile=clusters.pdf
acroread clusters.pdf &

# png
Rscript MDplot/tests/MDplot_bash.R MDplot_ramachandran files=MDplot/data/MDplot_ramachandran_example.txt title="whatever ramachandran" size=1400,1400 resolution=175 outformat=png outfile=ramachandran.png
eog ramachandran.png &

# tiff
Rscript MDplot/tests/MDplot_bash.R MDplot_hbond files=MDplot/data/MDplot_hbond_example.tar.gz title="the hbond thingy here" size=1200,900 resolution=165 outformat=tiff outfile=hbond.tiff
eog hbond.tiff &
