
#!/bin/bash

# clusters
#Rscript MDplot/bash/MDplot_bash.R clusters files=MDplot/inst/extdata/clusters_example.txt title="Cluster analysis" size=900,900 outformat=pdf outfile=clusters.pdf clustersNumber=7 trajectoryNames=WT,variant1a,variantb,variant2,variant3,variant4
#Rscript MDplot/bash/MDplot_bash.R clusters help=TRUE
#acroread clusters.pdf &

# ramachandran
#Rscript MDplot/bash/MDplot_bash.R ramachandran files=MDplot/inst/extdata/ramachandran_example.txt title="Ramachandran plot" size=1400,1400 resolution=175 outformat=tiff outfile=ramachandran.tiff angleColumns=1,2 bins=125,125 heatFun=norm printLegend=TRUE plotType=fancy
#Rscript MDplot/bash/MDplot_bash.R ramachandran help=TRUE
#eog ramachandran.tiff &

# hbond
#Rscript MDplot/bash/MDplot_bash.R hbond files=MDplot/inst/extdata/hbond_example.txt title="Hbonds plot" size=1200,900 resolution=165 outformat=tiff outfile=hbond.tiff
#Rscript MDplot/bash/MDplot_bash.R hbond help=TRUE
#eog hbond.tiff &

# TIcurve
Rscript MDplot/bash/MDplot_bash.R TIcurve files=MDplot/inst/extdata/TIcurve_fb_forward_example.txt,MDplot/inst/extdata/TIcurve_fb_backward_example.txt title="TIcurve plotted" size=900,900 invertedBackwards=TRUE resolution=145 outformat=png outfile=TIcurve.png
Rscript MDplot/bash/MDplot_bash.R TIcurve help=TRUE
eog TIcurve.png &

# RMSD
#Rscript MDplot/bash/MDplot_bash.R rmsd files=MDplot/inst/extdata/MDplot_rmsd1_example.txt,MDplot/inst/extdata/MDplot_rmsd2_example.txt title="RMSD" size=900,900 resolution=100 outformat=tiff outfile=RMSD.tiff datanames=wtSIM,mutSIM
#Rscript MDplot/bash/MDplot_bash.R rmsd help=TRUE
#eog RMSD.tiff &

# RMSF
#Rscript MDplot/bash/MDplot_bash.R rmsf files=MDplot/inst/extdata/MDplot_rmsf1_example.txt,MDplot/inst/extdata/MDplot_rmsf2_example.txt title="RMSF" size=900,750 resolution=125 outformat=tiff outfile=RMSF.tiff
#Rscript MDplot/bash/MDplot_bash.R rmsf help=TRUE
#eog RMSF.tiff &

# DSSP summary
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_summary files=MDplot/inst/extdata/MDplot_DSSP_summary_example.txt title="DSSP summary" size=900,750 outformat=pdf outfile=DSSP_summary.pdf printlegend=TRUE
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_summary help=TRUE
#acroread DSSP_summary.pdf &

# DSSP timeseries
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_timeseries files=MDplot/inst/extdata title="DSSP timeseries" size=1200,750 outformat=tiff outfile=DSSP_timeseries.tiff
#Rscript MDplot/bash/MDplot_bash.R MDplot_DSSP_timeseries help=TRUE
#eog DSSP_timeseries.tiff &

# XRMSD
#Rscript MDplot/bash/MDplot_bash.R MDplot_XRMSD files=MDplot/inst/extdata/MDplot_XRMSD_example.txt title="XRMSD" size=1100,900 outformat=pdf outfile=XRMSD.pdf
#Rscript MDplot/bash/MDplot_bash.R MDplot_XRMSD help=TRUE
#acroread XRMSD.pdf &

# hbond timeseries
#Rscript MDplot/bash/MDplot_bash.R MDplot_hbond_timeseries files=MDplot/inst/extdata/MDplot_hbond_timeseries_example.txt,MDplot/inst/extdata/MDplot_hbond_example.txt acceptors=61,64 donors=64,72 title="HBOND timeseries" size=1450,950 plotoccurences=TRUE outformat=tiff printnames=TRUE outfile=hbond_timeseries.tiff single=TRUE timeNS=TRUE
#Rscript MDplot/bash/MDplot_bash.R MDplot_hbond_timeseries help=TRUE
#eog hbond_timeseries.tiff

# clusters timeseries
#Rscript MDplot/bash/MDplot_bash.R MDplot_clusters_timeseries files=MDplot/inst/extdata/MDplot_clusters_timeseries_example.txt title="CLUSTERS timeseries" size=1200,1000 outformat=png outfile=clusters_timeseries.png timeNS=TRUE lengths=4000,4000,4000,4000,4000,4000 clusternumber=9
#Rscript MDplot/bash/MDplot_bash.R MDplot_clusters_timeseries help=TRUE
#eog clusters_timeseries.png
