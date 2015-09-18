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
STRING_function <- VEC_inputArguments[ 1 ]
VEC_arguments <- parse_arguments( VEC_inputArguments[ -1 ] ) # -1: function name excluding
#########

# prepare vectors for checks
VEC_requiredForAll <- c( "files" )
VEC_allowedForAll <- c( VEC_requiredForAll, "size", "outformat",
                        "outfile", "title", "subtitle",
                        "xaxislable", "yaxislable", "enableprotocol",
                        "colours" )
#########
  
# check, which plot has been selected
if( STRING_function == "MDplot_DSSP_summary" )
{
}
if( STRING_function == "MDplot_RMSF" )
{
  # check, if input is sane for this plot and get input files
  testRequired( c( VEC_requiredForAll ), getListOfKeys( VEC_arguments ) )
  testAllowed( c( VEC_allowedForAll ), getListOfKeys( VEC_arguments ) )
  VEC_files <- getFiles( getValue( VEC_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_RMSF( MDplot_load_rmsf( VEC_files ) )
}
if( STRING_function == "MDplot_RMSD" )
{
}
if( STRING_function == "MDplot_TIcurve" )
{
}
if( STRING_function == "MDplot_clusters" )
{
  # check, if input is sane for this plot and get input files
  testRequired( c( VEC_requiredForAll ), getListOfKeys( VEC_arguments ) )
  testAllowed( c( VEC_allowedForAll ), getListOfKeys( VEC_arguments ) )
  VEC_files <- getFiles( getValue( VEC_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_clusters( MDplot_load_clusters( VEC_files ) )
}
if( STRING_function == "MDplot_DSSP_timeseries" )
{
}
if( STRING_function == "MDplot_hbond" )
{
  # check, if input is sane for this plot and get input files
  testRequired( c( VEC_requiredForAll ), getListOfKeys( VEC_arguments ) )
  testAllowed( c( VEC_allowedForAll ), getListOfKeys( VEC_arguments ) )
  VEC_files <- getFiles( getValue( VEC_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_hbond( MDplot_load_hbond( VEC_files ) )
}
#########
