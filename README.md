# MDplot: Visualise Molecular Dynamics Analyses
MDplot is an R package that allows automated plot generation directly from the output produced by Molecular Dynamics (MD) engines. Currently, loading functions for [GROMOS](http://www.gromos.net), [GROMACS](http://www.gromacs.org) and [AMBER](http://www.ambermd.org) type of output is provided. See the [the MDplot publication](https://journal.r-project.org/archive/2017/RJ-2017-007/RJ-2017-007.pdf) for a full description.

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

A script including many examples is available in the package (path: "MDplot/inst/bash/test.sh").

## Additional information and examples
Loading functions parse input and translate it into data structures that can be read by the respective plotting functions.

```
dssp( load_dssp( system.file( "extdata/dssp_example.txt.gz",
                 package = "MDplot" ) ) )
```

The type of molecular dynamics engine used can be set by parameter ```mdEngine``` for both the bash interface and the call from within R:

```
xrmsd( load_xrmsd( system.file( "extdata/xrmsd_example_AMBER.txt.gz",
                                package = "MDplot" ),
                   mdEngine = "AMBER" ),
       rmsdUnit = "angstrom" )
```

### Example pictures
![MDplot::ramachandran() example](http://margreitter.com/store/MDplot_github_ramachandran.png)
![MDplot::clusters_ts() example](http://margreitter.com/store/MDplot_github_clusters_ts.png)
![MDplot::hbond_ts() example](http://margreitter.com/store/MDplot_github_hbond_ts.png)

## New in the latest major version
Version: 1.1.1
Date: 2019-08-25

* changed selection of hydrogen bond IDs for 'hbond_ts()'

Version: 1.0.1
Date: 2017-07-04

* fixed some typos in the manual pages
* added vignette (publication)
* added special input support for function "load_timeseries()" to be able
  to load multi-column timeseries data

Version: 1.0.0
Date: 2017-02-24

* fixed issue with proper residue display when sub-selection was done in function 'dssp()'
* fixed issue with bin-expansion in function 'load_noe()'
* removed unnecessary input parameters from several functions
* added 'stride' to function 'load_dssp_ts()'
* changed parameters of function 'load_rmsf()'
* renamed functions 'load_dssp_summary()' and 'dssp_summary()' into
  'load_dssp()' and 'dssp()', respectively
* added additional box lines to function 'ramachandran()' for
  'plotType = "comic"'
* changed colour coding to be more consistent in function 'ramachandran()'
* added 'barScaleFactor' to function 'dssp_ts()' to account for the
  possibility that very small peptides might be plotted
* added GROMACS support for functions: 'load_dssp()', 'load_dssp_ts()',
  'load_hbond()', 'load_hbond_ts()', 'load_clusters()',
  'load_clusters_ts()', 'load_ramachandran()' and 'load_xrmsd()'
* added AMBER support for functions: 'load_rmsd()', 'load_ramachandran()',
  'load_dssp()', 'load_dssp_ts()', 'load_hbond()', 'load_clusters()',
  'load_clusters_ts()', 'load_rmsf()' and 'load_xrmsd()'
* added function 'load_XPM()' to load "X PixMap" data as commonly
  produced by GROMACS
* added functions 'find_Nth_occurrence()', 'split_GROMACS_atomnames()' and
  'split_AMBER_atomnames()' to the collection of helper functions
* added parameter 'showMultipleInteractions' to function 'hbond()' to
  represent multi- or hybrid hydrogen bond donors and acceptors
* fixed time-axis issue in functions 'rmsd()' and 'rmsf()'
* changed all names on example input files according to convention

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

## Outlook (probably)
1. support for other MD engines such as NAMD, CHARMM, ...
2. support DISICL input (both C++ and phyton implementation)
3. provide more plotting
4. support multiple rmsd average curves at the same time
