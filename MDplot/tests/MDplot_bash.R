# load required libraries
library( R.utils )
library( "MDplot" )
#########

# load required files
#suppressMessages( sourceDirectory( "R/", modifiedOnly = FALSE ) )
#########

# get arguments and look for function call
VEC_arguments <- commandArgs( trailingOnly = TRUE )
if( length( VEC_arguments ) < 1 )
{
  stop( "Error due to missing arguments. You need to supply at least an input file: input='/path/to/file'" )
}
LIST_arguments <- parse_arguments( VEC_arguments )
if( VEC_arguments[ 1 ] == "MDplot_DSSP_summary" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_RMSF" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_RMSD" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_TIcurve" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_clusters" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_DSSP_timeseries" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_hbond" )
{
}
