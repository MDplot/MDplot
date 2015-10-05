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
                        "colours", "resolution", "axisnames",
                        "datanames", "printlegend" )
#########

# set settings for all plots to be followed
BOOL_printLegend = TRUE
if( isKeySet( LIST_arguments, "printlegend" ) )
  if( getValue( LIST_arguments, "printlegend" ) == "FALSE" )
    BOOL_printLegend = FALSE
VEC_size <- c( 640, 640 )
if( isKeySet( LIST_arguments, "size" ) )
  VEC_size <- unlist( strsplit( getValue( LIST_arguments, "size" ),
                                ",",
                                fixed = TRUE ) )
VEC_dataNames <- NULL
if( isKeySet( LIST_arguments, "datanames" ) )
  VEC_dataNames <- unlist( strsplit( getValue( LIST_arguments, "datanames" ),
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
                       BOOL_printLegend = BOOL_printLegend,
                       main = ifelse( isKeySet( LIST_arguments, "title" ),
                                      getValue( LIST_arguments, "title" ),
                                      NULL ) )
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
                          main = ifelse( isKeySet( LIST_arguments, "title" ),
                                         getValue( LIST_arguments, "title" ),
                                         NULL ) )
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
                main = ifelse( isKeySet( LIST_arguments, "title" ),
                               getValue( LIST_arguments, "title" ),
                               NULL ),
                xlab = ifelse( is.null( VEC_axisNames ), "time [structure]", VEC_axisNames[ 1 ] ),
                ylab = ifelse( is.null( VEC_axisNames ), "time [structure]", VEC_axisNames[ 2 ] ) )
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
               VEC_names = VEC_dataNames,
               main = ifelse( isKeySet( LIST_arguments, "title" ),
                              getValue( LIST_arguments, "title" ),
                              NULL ) )
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
               VEC_names = VEC_dataNames,
               main = ifelse( isKeySet( LIST_arguments, "title" ),
                              getValue( LIST_arguments, "title" ),
                              NULL ) )
}



if( STRING_function == "MDplot_ramachandran" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "bins" ), LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  VEC_bins <- c( 450, 450 )
  if( isKeySet( LIST_arguments, "bins" ) )
    VEC_bins <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "bins" ),
                                              ",",
                                              fixed = TRUE ) ) )
  
  # plot
  MDplot_ramachandran( MDplot_load_ramachandran( VEC_files ),
                       xbins = VEC_bins[ 1 ],
                       ybins = VEC_bins[ 2 ],
                       main = ifelse( isKeySet( LIST_arguments, "title" ),
                                      getValue( LIST_arguments, "title" ),
                                      NULL ) )
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
                  main = ifelse( isKeySet( LIST_arguments, "title" ),
                                 getValue( LIST_arguments, "title" ),
                                 NULL ) )
}



if( STRING_function == "MDplot_clusters" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "clusternumber",
                  "trajectorynames" ),
               LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  
  # load matrix and set names
  MAT_input <- MDplot_load_clusters( VEC_files )
  if( isKeySet( LIST_arguments, "trajectorynames" ) )
  {
    VEC_names <- unlist( strsplit( getValue( LIST_arguments, "trajectorynames" ),
                                   ",",
                                   fixed = TRUE ) )
    if( length( VEC_names ) != nrow( MAT_input ) )
      stop( paste( "Error while assigning user specified trajectory ",
                   "names, since the numbers (",
                   length( VEC_names ),
                   ",",
                   nrow( MAT_input ),
                   ") do not match.",
                   sep = "" ) )
    rownames( MAT_input ) <- VEC_names
  }

  # plot
  MDplot_clusters( MAT_input,
                   INT_numberClusters = ifelse( isKeySet( LIST_arguments, "clusternumber" ),
                                                getValue( LIST_arguments, "clusternumber" ),
                                                NULL ),
                   BOOL_ownTrajectoryNames = ifelse( isKeySet( LIST_arguments,
                                                               "trajectorynames" ),
                                                     TRUE,
                                                     FALSE ),
                   xlab = ifelse( is.null( VEC_axisNames ), "clusters", VEC_axisNames[ 1 ] ),
                   ylab = ifelse( is.null( VEC_axisNames ), "# configurations", VEC_axisNames[ 2 ] ),
                   main = ifelse( isKeySet( LIST_arguments, "title" ),
                                  getValue( LIST_arguments, "title" ),
                                  NULL ) )
}



if( STRING_function == "MDplot_clusters_timeseries" )
{
  # check, if input is sane for this plot and get input files
  testRequired( c( VEC_requiredForAll,
                   "lengths" ),
                LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "clusternumber",
                  "trajectorynames",
                  "timeNS",
                  "snapshotsNS",
                  "lengths" ),
               LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  
  # load matrix and set names
  LIST_input <- MDplot_load_clusters_timeseries( STRING_path = VEC_files,
                                                VEC_lengths = as.numeric( unlist( strsplit( getValue( LIST_arguments,
                                                                                           "lengths" ),
                                                                                           split = ",",
                                                                                           fixed = TRUE ) ) ),
                                                VEC_names = NULL )
  if( isKeySet( LIST_arguments, "trajectorynames" ) )
  {
    VEC_names <- unlist( strsplit( getValue( LIST_arguments, "trajectorynames" ),
                                   ",",
                                   fixed = TRUE ) )
    if( length( VEC_names ) != length( LIST_input ) )
      stop( paste( "Error while assigning user specified trajectory ",
                   "names, since the numbers (",
                   length( VEC_names ),
                   ",",
                   length( LIST_input ),
                   ") do not match.",
                   sep = "" ) )
    for( i in 1:length( LIST_input ) )
      LIST_input[[ i ]][[ 1 ]] <- VEC_names[ i ]
  }
  
  # plot
  MDplot_clusters_timeseries( LIST_input,
                              INT_numberClusters = ifelse( isKeySet( LIST_arguments, "clusternumber" ),
                                                           as.numeric( getValue( LIST_arguments, "clusternumber" ) ),
                                                           NA ),
                              BOOL_printNanoseconds = ifelse( isKeySet( LIST_arguments, "timeNS" ),
                                                              TRUE,
                                                              FALSE ),
                              REAL_snapshotsNS = ifelse( isKeySet( LIST_arguments, "snapshotsperNS" ),
                                                         as.numeric( getValue( LIST_arguments, "snapshotsperNS" ) ),
                                                         1000 ),
                              main = ifelse( isKeySet( LIST_arguments, "title" ),
                                             getValue( LIST_arguments, "title" ),
                                             NULL ) )
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
                main = ifelse( isKeySet( LIST_arguments, "title" ),
                               getValue( LIST_arguments, "title" ),
                               NULL ) )
}



if( STRING_function == "MDplot_hbond_timeseries" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "plotoccurences",
                  "acceptors",
                  "donors",
                  "printnames",
                  "single",
                  "timeNS",
                  "snapshotsperNS" ),
               LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  if( length( VEC_files ) != 2 )
    stop( paste( "Error in file checking: seemingly, the number of provided files is",
                 length( VEC_files ), "and not 2, as expected." ) )
  VEC_acceptors = NULL
  if( isKeySet( LIST_arguments, "acceptors" ) )
    VEC_acceptors <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "acceptors" ),
                                                   ",",
                                                   fixed = TRUE ) ) )
  VEC_donors <- NULL
  if( isKeySet( LIST_arguments, "donors" ) )
    VEC_donors <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "donors" ),
                                                ",",
                                                fixed = TRUE ) ) )
  
  # plot
  MDplot_hbond_timeseries( MDplot_load_hbond_timeseries( VEC_files[ 1 ] ),
                           MDplot_load_hbond( VEC_files[ 2 ] ),
                           BOOL_printNames = ifelse( isKeySet( LIST_arguments, "printnames" ),
                                                     TRUE,
                                                     FALSE ),
                           BOOL_plotOccurences = ifelse( isKeySet( LIST_arguments, "plotoccurences" ),
                                                         TRUE,
                                                         FALSE ),
                           VEC_acceptorRange = VEC_acceptors,
                           VEC_donorRange = VEC_donors,
                           BOOL_namesToSingle = ifelse( isKeySet( LIST_arguments, "single" ),
                                                        TRUE,
                                                        FALSE ),
                           BOOL_timeInNS = ifelse( isKeySet( LIST_arguments, "timeNS" ),
                                                  TRUE,
                                                  FALSE ),
                           REAL_divisionFactor = ifelse( isKeySet( LIST_arguments,
                                                                   "snapshotsperNS" ),
                                                         getValue( LIST_arguments,
                                                                   "snapshotsperNS" ),
                                                         1000 ),
                           main = ifelse( isKeySet( LIST_arguments, "title" ),
                                          getValue( LIST_arguments, "title" ),
                                          NULL ) )
}
#########

# end
dev.off()
