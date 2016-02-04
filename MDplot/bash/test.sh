
#!/bin/bash

# clusters
#Rscript MDplot_bash.R clusters files=../inst/extdata/clusters_example.txt title="Cluster analysis" size=900,900 outformat=pdf outfile=clusters.pdf clustersNumber=7 names=WT,variant1a,variantb,variant2,variant3,variant4
#Rscript MDplot_bash.R clusters help=TRUE
#acroread clusters.pdf &

# ramachandran
#Rscript MDplot_bash.R ramachandran files=../inst/extdata/ramachandran_example.txt title="Ramachandran plot" size=1400,1400 resolution=175 outformat=tiff outfile=ramachandran.tiff angleColumns=1,2 bins=75,75 heatFun=norm printLegend=TRUE plotType=fancy
#Rscript MDplot_bash.R ramachandran help=TRUE
#eog ramachandran.tiff &

# hbond
#Rscript MDplot/bash/MDplot_bash.R hbond files=MDplot/inst/extdata/hbond_example.txt title="Hbonds plot" size=1200,900 resolution=165 outformat=tiff outfile=hbond.tiff
#Rscript MDplot/bash/MDplot_bash.R hbond help=TRUE
#eog hbond.tiff &

# TIcurve
#Rscript MDplot_bash.R TIcurve files=../inst/extdata/TIcurve_fb_forward_example.txt,../inst/extdata/TIcurve_fb_backward_example.txt title="TIcurve plotted" size=900,900 invertedBackwards=TRUE resolution=145 outformat=png outfile=TIcurve.png
#Rscript MDplot_bash.R TIcurve help=TRUE
#eog TIcurve.png &

# RMSD
#Rscript MDplot_bash.R rmsd files=../inst/extdata/rmsd1_example.txt,../inst/extdata/rmsd2_example.txt title="RMSD" size=900,900 resolution=100 rmsdUnit=Angstrom outformat=tiff outfile=RMSD.tiff names=wtSIM,mutSIM
#Rscript MDplot_bash.R rmsd help=TRUE
#eog RMSD.tiff &

# RMSF
#Rscript MDplot_bash.R rmsf files=../inst/extdata/rmsf1_example.txt,../inst/extdata/rmsf2_example.txt title="RMSF" range=30,90 outformat=tiff outfile=RMSF.tiff residuewise=TRUE
#Rscript MDplot_bash.R rmsf help=TRUE
#eog RMSF.tiff &

# DSSP summary
# done
#Rscript MDplot_bash.R dssp_summary files=../inst/extdata/dssp_summary_example.txt title="DSSP summary" size=900,750 outformat=pdf outfile=DSSP_summary.pdf printLegend=TRUE plotType=bars
#Rscript MDplot_bash.R dssp_summary help=TRUE
#acroread DSSP_summary.pdf &

# DSSP timeseries
#Rscript MDplot_bash.R dssp_ts files=../inst/extdata title="DSSP timeseries" size=1200,750 timeUnit=ns snapshotsPerTimeInt=1000 residueBoundaries=21,70 outformat=pdf outfile=DSSP_timeseries.pdf
#Rscript MDplot_bash.R dssp_ts help=TRUE
#acroread DSSP_timeseries.pdf &

# XRMSD
#Rscript MDplot_bash.R xrmsd files=../inst/extdata/xrmsd_example.txt title="XRMSD" size=1100,900 outformat=pdf outfile=XRMSD.pdf xaxisRange=75,145
#Rscript MDplot_bash.R xrmsd help=TRUE
#acroread XRMSD.pdf &

# hbond timeseries
#Rscript MDplot_bash.R MDplot_hbond_timeseries files=../inst/extdata/MDplot_hbond_timeseries_example.txt,../inst/extdata/MDplot_hbond_example.txt acceptors=61,64 donors=64,72 title="HBOND timeseries" size=1450,950 plotoccurences=TRUE outformat=tiff printnames=TRUE outfile=hbond_timeseries.tiff single=TRUE
#Rscript MDplot_bash.R MDplot_hbond_timeseries help=TRUE
#eog hbond_timeseries.tiff

# clusters timeseries
#Rscript MDplot_bash.R clusters_ts files=../inst/extdata/clusters_ts_example.txt title="CLUSTERS timeseries" size=1200,1000 outformat=png outfile=clusters_ts.png timeUnit=ns lengths=4000,4000,4000,4000,4000,4000 clustersNumber=9
#Rscript MDplot_bash.R clusters_ts help=TRUE
#eog clusters_ts.png
