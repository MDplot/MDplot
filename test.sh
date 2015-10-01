
#!/bin/bash

# clusters
#Rscript MDplot/bash/MDplot_bash.R MDplot_clusters files=MDplot/inst/extdata/MDplot_clusters_example.txt title="Cluster analysis" size=900,900 outformat=pdf outfile=clusters.pdf clusternumber=7 trajectorynames=WT,variant1a,variantb,variant2,variant3,variant4
#acroread clusters.pdf &

# ramachandran
#Rscript MDplot/bash/MDplot_bash.R MDplot_ramachandran files=MDplot/inst/extdata/MDplot_ramachandran_example.txt title="Ramachandran plot" size=1400,1400 resolution=175 outformat=png outfile=ramachandran.png
#eog ramachandran.png &

# hbond
#Rscript MDplot/bash/MDplot_bash.R MDplot_hbond files=MDplot/inst/extdata/MDplot_hbond_example.txt title="Hbonds plot" size=1200,900 resolution=165 outformat=tiff outfile=hbond.tiff
#eog hbond.tiff &

# TIcurve
#Rscript MDplot/bash/MDplot_bash.R MDplot_TIcurve files=MDplot/inst/extdata/MDplot_TIcurve_example.txt title="TIcurve plotted" size=900,900 resolution=145 outformat=png outfile=TIcurve.png
#eog TIcurve.png &

# RMSD
#Rscript MDplot/bash/MDplot_bash.R MDplot_RMSD files=MDplot/inst/extdata/MDplot_rmsd1_example.txt,MDplot/inst/extdata/MDplot_rmsd2_example.txt title="RMSD" size=900,900 resolution=100 outformat=tiff outfile=RMSD.tiff datanames=wtSIM,mutSIM
#eog RMSD.tiff &

# RMSF
#Rscript MDplot/bash/MDplot_bash.R MDplot_RMSF files=MDplot/inst/extdata/MDplot_rmsf1_example.txt,MDplot/inst/extdata/MDplot_rmsf2_example.txt title="RMSF" size=900,750 resolution=125 outformat=tiff outfile=RMSF.tiff
#eog RMSF.tiff &

# DSSP summary
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_summary files=MDplot/inst/extdata/MDplot_DSSP_summary_example.txt title="DSSP summary" size=900,750 outformat=pdf outfile=DSSP_summary.pdf printlegend=TRUE
#acroread DSSP_summary.pdf &

# DSSP timeseries
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_timeseries files=MDplot/inst/extdata title="DSSP timeseries" size=1200,750 outformat=tiff outfile=DSSP_timeseries.tiff
#eog DSSP_timeseries.tiff &

# XRMSD
#Rscript MDplot/bash/MDplot_bash.R MDplot_XRMSD files=MDplot/inst/extdata/MDplot_XRMSD_example.txt title="XRMSD" size=1100,900 outformat=pdf outfile=XRMSD.pdf
#acroread XRMSD.pdf &

# hbond timeseries
#Rscript MDplot/bash/MDplot_bash.R MDplot_hbond_timeseries files=MDplot/inst/extdata/MDplot_hbond_timeseries_example.txt,MDplot/inst/extdata/MDplot_hbond_example.txt acceptors=61,64 donors=64,72 title="HBOND timeseries" size=1450,950 plotoccurences=TRUE outformat=tiff printnames=TRUE outfile=hbond_timeseries.tiff single=TRUE timeNS=TRUE
#eog hbond_timeseries.tiff
