install.packages( "./MDplot_1.0.tar.gz", dependencies = TRUE,
                  repos = NULL, type = "source" )
library( MDplot )

# get arguments and look for function call
VEC_inputArguments <- commandArgs( trailingOnly = TRUE )
if( length( VEC_inputArguments ) < 2 )
{
  stop( paste( "Error due to missing arguments. You need to supply at least a ",
               "plot selection and an input file, e.g. 'Rscript /path/to/file/MDplot_bash.R ",
               "MDplot_RMSF files=/path/to/file/file1.txt,/path/to/file/file2.txt'" ) )
}
VEC_arguments <- parse_arguments( VEC_inputArguments )
VEC_keys <- c()
for( i in 1:length( VEC_arguments ) )
{
  VEC_keys[ i ] <- slot( unlist( VEC_arguments[[ i ]] ), "key" )
}
print( VEC_keys )
if( VEC_arguments[ 1 ] == "MDplot_DSSP_summary" )
{
}
if( VEC_arguments[ 1 ] == "MDplot_RMSF" )
{
  VEC_required_input <- c( "files" )
  for( i in 1:length( VEC_required_input ) )
  {
    #if( !( VEC_required_input[ i ] %in% VEC_arguments[[ key ]] ) )
    #{
    #}
  }
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
