# MDplot: Visualize Molecular Dynamics Analyses
MDplot is an R package that allows automated plot generation directly from the output produced by Molecular Dynamics (MD) engines. Currently, loading functions for [GROMOS](http://www.gromos.net) type of output is provided.

## Call from within bash script
An Rscript interface is provided, allowing to set most options:
```
#!/bin/bash
# analysis program (e.g. clustering algorithm)
Rscript MDplot_bash.R clusters \
                 files=clusters_example.txt.gz \
                 title="clust" size=900,900 \
                 outformat=pdf \
                 clustersNumber=7 \
                 ...
```

## Additional information and examples
See XXX

## New in the latest major version
Version: 0.3.1
Date: 2016-04-28

* added 'printErrors' flag to function 'TIcurve()'
* added 'errorBarThreshold' flag to function 'TIcurve()'
* fixed bug in function 'ramachandran()' for type "fancy"
* added return information to functions where appropriate
* fixed 'xrmsd()' ellipsis
* changed some phrases and the title

Version: 0.3  
Date: 2016-02-15

* added multiple vector selection by hydrogen bond identifiers to function 'hbond_ts()'
* added argument 'printValues' functionality to function 'TIcurve()'
* removed hard-coding for the filenames of function 'load_dssp_ts()'
* included new functions 'load_timeseries()' and 'timeseries()'
* added argument 'legendPosition' to functions 'rmsd()' and 'rmsf()'
* added labels for axes for function 'xrmsd()'
* renamed title ("Analyses") to comply with the publication title

## Outlook
1. support for other MD engines such as GROMACS, NAMD, CHARMM, ...
2. support DISICL input (both for C++ and phyton implementation)
3. support for average-RMSD input resulting from replicate input
