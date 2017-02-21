# MDplot: Visualise Molecular Dynamics Analyses
MDplot is an R package that allows automated plot generation directly from the output produced by Molecular Dynamics (MD) engines. Currently, loading functions for [GROMOS](http://www.gromos.net), [GROMACS](http://www.gromacs.org) and [AMBER](http://www.ambermd.org) type of output is provided. See the PDF for a full description.

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
Loading functions parse input and translate it into data structures that can be read by the respective plotting functions.

```
dssp( load_dssp( system.file( "extdata/dssp_example.txt.gz",
                 package = "MDplot" ) ) )
```

## New in the latest major version
Version: 0.4.0
Date: 2017-02-24

o removed unnecessary input parameters from several functions
o added 'stride' to function 'load_dssp_ts()'
o changed parameters of function 'load_rmsf()'
o renamed functions 'load_dssp_summary()' and 'dssp_summary()' into
  'load_dssp()' and 'dssp()', respectively
o added additional box lines to function 'ramachandran()' for
  'plotType = "comic"'
o added 'barScaleFactor' to function 'dssp_ts()' to account for the
  possibility that very small peptides might be plotted
o added GROMACS support for functions: 'load_dssp()', 'load_dssp_ts()',
  'load_hbond()', 'load_hbond_ts()', 'load_clusters()',
  'load_clusters_ts()', 'load_ramachandran()' and 'load_xrmsd()'
o added AMBER support for functions: 'load_rmsd()', 'load_ramachandran()',
  'load_dssp()', 'load_dssp_ts()', 'load_hbond()', 'load_clusters()',
  'load_clusters_ts()', 'load_rmsf()' and 'load_xrmsd()'
o added function 'load_XPM()' to load "X PixMap" data as commonly
  produced by GROMACS
o added functions 'find_Nth_occurrence()', 'split_GROMACS_atomnames()' and
  'split_AMBER_atomnames()' to the collection of helper functions
o added parameter 'showMultipleInteractions' to function 'hbond()' to
  represent multi- or hybrid hydrogen bond donors and acceptors
o fixed time-axis issue in functions 'rmsd()' and 'rmsf()'


Version: 0.3.1
Date: 2016-04-28

* added 'printErrors' flag to function 'TIcurve()'
* added 'errorBarThreshold' flag to function 'TIcurve()'
* fixed bug in function 'ramachandran()' for type "fancy"
* added return information to functions where appropriate
* fixed 'xrmsd()' ellipsis
* changed some phrases and the title

Version: 0.3.0
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
