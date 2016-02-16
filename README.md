# MDplot: Visualize Molecular Dynamics Analyses
MDplot is an R package that allows automated plot generation directly from the output produced by Molecular Dynamics (MD) engines. Currently, loading functions for GROMOS type of input is provided.

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

## Outlook
1. support for other MD engines such as GROMACS, NAMD, CHARMM, ...
2. support DISICL input (both for C++ and phyton implementation)
3. support for average-RMSD input resulting from replicate input
