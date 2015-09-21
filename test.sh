#!/bin/bash

# clusters
Rscript MDplot/tests/MDplot_bash.R MDplot_clusters files=MDplot/data/MDplot_clusters_example.txt title="cluster try" size=1200,900 outformat=pdf outfile=clusters.pdf
acroread clusters.pdf &

# ramachandran
Rscript MDplot/tests/MDplot_bash.R MDplot_ramachandran files=MDplot/data/MDplot_ramachandran_example.txt title="whatever ramachandran" size=1400,1400 resolution=175 outformat=png outfile=ramachandran.png
eog ramachandran.png &

# hbond
Rscript MDplot/tests/MDplot_bash.R MDplot_hbond files=MDplot/data/MDplot_hbond_example.tar.gz title="the hbond thingy here" size=1200,900 resolution=165 outformat=tiff outfile=hbond.tiff
eog hbond.tiff &

# TIcurve
Rscript MDplot/tests/MDplot_bash.R MDplot_TIcurve files=MDplot/data/MDplot_TIcurve_example.txt title="TIcurve for the win" size=900,900 resolution=145 outformat=png outfile=TIcurve.png
eog TIcurve.png &

# RMSD
Rscript MDplot/tests/MDplot_bash.R MDplot_RMSD files=MDplot/data/MDplot_rmsd1_example.txt,MDplot/data/MDplot_rmsd2_example.txt title="RMSD yeah!" size=650,450 resolution=100 outformat=tiff outfile=RMSD.tiff
eog RMSD.tiff &

# RMSF
Rscript MDplot/tests/MDplot_bash.R MDplot_RMSF files=MDplot/data/MDplot_rmsf1_example.txt,MDplot/data/MDplot_rmsf2_example.txt title="RMSF yeah!!!" size=1650,1450 resolution=125 outformat=tiff outfile=RMSF.tiff
eog RMSF.tiff &

# DSSP summary
Rscript MDplot/tests/MDplot_bash.R MDplot_DSSP_summary files=MDplot/data/MDplot_DSSP_summary_example.txt title="DSSP finally for now" size=900,750 outformat=pdf outfile=DSSP_summary.pdf
acroread DSSP_summary.pdf &
