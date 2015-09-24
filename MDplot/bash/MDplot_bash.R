require( MDplot )

# get arguments and look for function call
VEC_inputArguments <- commandArgs( trailingOnly = TRUE )
if( length( VEC_inputArguments ) < 2 )
{
  stop( "Error due to missing arguments. You need to supply at least a plot selection and an input file." )
}
STRING_function <- VEC_inputArguments[ 1 ]
LIST_arguments <- parse_arguments( VEC_inputArguments[ -1 ] ) # -1: function name excluding
#########

# prepare vectors for checks
VEC_requiredForAll <- c( "files" )
VEC_allowedForAll <- c( VEC_requiredForAll, "size", "outformat",
                        "outfile", "title", "subtitle",
                        "xaxislabel", "yaxislabel", "enableprotocol",
                        "colours", "resolution", "axisnames" )
#########

# set settings for all plots to be followed
VEC_size <- c( 640, 640 )
if( isKeySet( LIST_arguments, "size" ) )
  VEC_size <- unlist( strsplit( getValue( LIST_arguments, "size" ),
                                ",",
                                fixed = TRUE ) )
VEC_axisNames <- NULL
if( isKeySet( LIST_arguments, "axisnames" ) )
  VEC_axisNames <- unlist( strsplit( getValue( LIST_arguments, "axisnames" ),
                                     ",",
                                     fixed = TRUE ) )
STRING_outformat <- "pdf"
if( isKeySet( LIST_arguments, "outformat" ) )
  STRING_outformat <- getValue( LIST_arguments, "outformat" )
STRING_main <- NULL
if( isKeySet( LIST_arguments, "title" ) )
  STRING_main <- getValue( LIST_arguments, "title" )
STRING_outfile <- "MDplot_out"
if( isKeySet( LIST_arguments, "outfile" ) )
  STRING_outfile <- getValue( LIST_arguments, "outfile" )
REAL_resolution <- 150
if( isKeySet( LIST_arguments, "resolution" ) )
  REAL_resolution <- as.numeric( getValue( LIST_arguments, "resolution" ) )
#########

# define plot device and options
if( STRING_outformat == "pdf" )
{
  pdf( file = paste( STRING_outfile ),
       width = as.numeric( VEC_size[ 1 ] ) / 96,
       height = as.numeric( VEC_size[ 2 ] ) / 96 )
}
if( STRING_outformat == "png" )
{
  png( paste( STRING_outfile ),
       width = as.numeric( VEC_size[ 1 ] ),
       height = as.numeric( VEC_size[ 2 ] ),
       units = "px",
       res = REAL_resolution,
       type = "cairo" )
}
if( STRING_outformat == "tiff" )
{
  tiff( filename = paste( STRING_outfile ),
        width = as.numeric( VEC_size[ 1 ] ),
        height = as.numeric( VEC_size[ 2 ] ),
        units = "px",
        compression = "none",
        res = REAL_resolution )
}
#########

# check, which plot has been selected
if( STRING_function == "MDplot_DSSP_summary" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_DSSP_summary( MDplot_load_DSSP_summary( VEC_files ),
                       main = ifelse( is.null( STRING_main ), "DSSP summary", STRING_main ) )
}
if( STRING_function == "MDplot_DSSP_timeseries" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_DSSP_timeseries( MDplot_load_DSSP_timeseries( VEC_files ),
                          main = ifelse( is.null( STRING_main ), "DSSP timeseries", STRING_main ) )
}
if( STRING_function == "MDplot_XRMSD" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_XRMSD( MDplot_load_XRMSD( VEC_files ),
                main = ifelse( is.null( STRING_main ), "XRMSD", STRING_main ) )
                xlab = ifelse( is.null( VEC_axisNames ), "snapshots", VEC_axisNames[ 1 ] ),
                ylab = ifelse( is.null( VEC_axisNames ), "snapshots", VEC_axisNames[ 2 ] ) )
}
if( STRING_function == "MDplot_RMSF" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_RMSF( MDplot_load_RMSF( VEC_files ),
               main = ifelse( is.null( STRING_main ), "RMSF", STRING_main ) )
}
if( STRING_function == "MDplot_RMSD" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_RMSD( MDplot_load_RMSD( VEC_files ),
               main = ifelse( is.null( STRING_main ), "RMSD", STRING_main ) )
}
if( STRING_function == "MDplot_ramachandran" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_ramachandran( MDplot_load_ramachandran( VEC_files ),
                       main = ifelse( is.null( STRING_main ), "Ramachandran plot", STRING_main ) )
}
if( STRING_function == "MDplot_TIcurve" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_TIcurve( MDplot_load_TIcurve( VEC_files ),
                  main = ifelse( is.null( STRING_main ), "TIcurve", STRING_main ) )
}
if( STRING_function == "MDplot_clusters" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_clusters( MDplot_load_clusters( VEC_files ),
                   main = ifelse( is.null( STRING_main ), "Clusters", STRING_main ) )
}
if( STRING_function == "MDplot_hbond" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_hbond( MDplot_load_hbond( VEC_files ),
                main = ifelse( is.null( STRING_main ), "Hbonds", STRING_main ) )
}
#########

# end
dev.off()
