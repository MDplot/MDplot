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
VEC_requiredForAll <- c(  )
VEC_allowedForAll <- c( VEC_requiredForAll, "files", "size", "outformat",
                        "outfile", "title", "subtitle",
                        "enableProtocol", "colours", "resolution",
                        "axisNames", "dataNames", "printLegend",
                        "help" )
VEC_allowedForAllDesc <- c( "<input file(s), separated by ','>", "<dimensions of the plot> (optional)", "['png'/'pdf'/'tiff'] (optional)",
                            "<outputfile> (optional)", "<plot main title> (optional)", "<plot subtitle> (optional)",
                            "<protocol steps in plot generation> (optional)", "<vector of colours used, separated by ','> (optional)", "<resolution> (optional)",
                            "<vector of names for the axes> (optional)", "<vector of names for the data sets> (optional)", "['true'/'false'] (optional)",
                            "<if set to 'true', all other options are ignored and help is printed> (optional)" )
#########

# set settings for all plots to be followed
BOOL_printLegend = TRUE
if( isKeySet( LIST_arguments, "printLegend" ) )
  if( getValue( LIST_arguments, "printLegend" ) == "FALSE" )
    BOOL_printLegend = FALSE
VEC_size <- c( 640, 640 )
if( isKeySet( LIST_arguments, "size" ) )
  VEC_size <- unlist( strsplit( getValue( LIST_arguments, "size" ),
                                ",",
                                fixed = TRUE ) )
VEC_dataNames <- NA
if( isKeySet( LIST_arguments, "dataNames" ) )
  VEC_dataNames <- unlist( strsplit( getValue( LIST_arguments, "dataNames" ),
                                ",",
                                fixed = TRUE ) )
VEC_axisNames <- NA
if( isKeySet( LIST_arguments, "axisNames" ) )
  VEC_axisNames <- unlist( strsplit( getValue( LIST_arguments, "axisNames" ),
                                     ",",
                                     fixed = TRUE ) )
STRING_outformat <- "png"
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
} else {
  if( STRING_outformat == "png" )
  {
    png( paste( STRING_outfile ),
         width = as.numeric( VEC_size[ 1 ] ),
         height = as.numeric( VEC_size[ 2 ] ),
         units = "px",
         res = REAL_resolution,
         type = "cairo" )
  } else {
    if( STRING_outformat == "tiff" )
    {
      tiff( filename = paste( STRING_outfile ),
            width = as.numeric( VEC_size[ 1 ] ),
            height = as.numeric( VEC_size[ 2 ] ),
            units = "px",
            compression = "none",
            res = REAL_resolution )
    } else {
      stop( paste( "Error, the specified output format '",
                   STRING_outformat,
                   "' is not known.",
                   sep = "" ) )
    }
  }
}
#########

# check, which plot has been selected
if( STRING_function == "MDplot_DSSP_summary" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "residues" ),
               LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  VEC_residues <- NA
  if( isKeySet( LIST_arguments, "residues" ) )
    VEC_residues <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "residues" ),
                                                  ",",
                                                  fixed = TRUE ) ) )
  
  # plot
  MDplot_DSSP_summary( MDplot_load_DSSP_summary( VEC_files ),
                       BOOL_printLegend = BOOL_printLegend,
                       VEC_showResidues = VEC_residues,
                       main = ifelse( isKeySet( LIST_arguments, "title" ),
                                      getValue( LIST_arguments, "title" ),
                                      NA ) )
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
                                         NA ) )
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
                               NA ),
                xlab = ifelse( is.na( VEC_axisNames ), "time [structure]", VEC_axisNames[ 1 ] ),
                ylab = ifelse( is.na( VEC_axisNames ), "time [structure]", VEC_axisNames[ 2 ] ) )
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
                              NA ) )
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
                              NA ) )
}



if( STRING_function == "MDplot_RMSD_average" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_allowedForAll, LIST_arguments )

  # plot
  MDplot_RMSD_average( LIST_input = list( list( name = "one", 
                                                files = getValue( LIST_arguments, "files" ) ) ),
                       main = ifelse( isKeySet( LIST_arguments, "title" ),
                                      getValue( LIST_arguments, "title" ),
                                      NA ) )
}



if( STRING_function == "MDplot_ramachandran" )
{
  # check, if input is sane for this plot and get input files
  VEC_ramaAll <- c( "bins",
                    "angleColumns",
                    "heatColumn",
                    "plotType",
                    "heatFun",
                    "heatUnits",
                    "plotContour",
                    "shiftAngles",
                    VEC_allowedForAll )
  if( isKeySet( LIST_arguments, "help" ) )
  {
    print_help( STRING_function,
                VEC_ramaAll,
                c( "<number of bins to divide data in (x, y)> (optional)",
                   "<columns in file containing dihedrals>",
                   "<column in file containing heat> (optional)",
                   "['sparse'/'comic'/'fancy'] (optional)",
                   "<function to treat heat with, default 'log'> (optional)",
                   "<units, in which heat is given> (optional)",
                   "<plot contour as well, default 'false'> (optional)",
                   "<if angle interval is not -180 to 180, a shift can be specified> (optional)",
                   VEC_allowedForAllDesc ) )
    quit( save = "no", status = 0, runLast = TRUE )
  }
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( VEC_ramaAll, LIST_arguments )
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
  VEC_angleColumns <- c( 1, 2 )
  if( isKeySet( LIST_arguments, "angleColumns" ) )
    VEC_angleColumns <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "angleColumns" ),
                                                      ",",
                                                      fixed = TRUE ) ) )
  STRING_heatfunction <- "log"
  if( isKeySet( LIST_arguments, "heatFun" ) )
    STRING_heatfunction <- getValue( LIST_arguments, "heatFun" )
  
  STRING_plotType <- "comic"
  if( isKeySet( LIST_arguments, "plotType" ) )
    STRING_plotType <- getValue( LIST_arguments, "plotType" )
  
  STRING_heatUnits <- NA
  if( isKeySet( LIST_arguments, "heatUnits" ) )
    STRING_heatUnits <- paste( "[", getValue( LIST_arguments, "heatUnits" ), "]", sep = "" )

  # plot
  MDplot_ramachandran( MDplot_load_ramachandran( VEC_files,
                                                 angleColumns = VEC_angleColumns,
                                                 shiftAngles = ifelse( isKeySet( LIST_arguments, "shiftAngles" ),
                                                                       as.numeric( getValue( LIST_arguments, "shiftAngles" ) ),
                                                                       NA ),
                                                 heatColumn = ifelse( isKeySet( LIST_arguments, "heatColumn" ),
                                                                      as.numeric( getValue( LIST_arguments, "heatColumn" ) ),
                                                                     NA ) ),
                       xBins = VEC_bins[ 1 ],
                       yBins = VEC_bins[ 2 ],
                       plotType = STRING_plotType,
                       heatFun = STRING_heatfunction,
                       printLegend = ifelse( isKeySet( LIST_arguments, "printLegend" ),
                                             TRUE,
                                             FALSE ),
                       plotContour = ifelse( isKeySet( LIST_arguments, "plotContour" ),
                                             TRUE,
                                             FALSE ),
                       heatUnits = STRING_heatUnits,
                       main = ifelse( isKeySet( LIST_arguments, "title" ),
                                      getValue( LIST_arguments, "title" ),
                                      NA ) )
}



if( STRING_function == "MDplot_TIcurve" )
{
  # check, if input is sane for this plot and get input files
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_allowedForAll,
                  "invert" ),
               LIST_arguments )
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
  {
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  }
  
  # plot
  MDplot_TIcurve( MDplot_load_TIcurve( VEC_files ),
                  BOOL_invertedBackwards = ifelse( isKeySet( LIST_arguments,
                                                             "invert" ),
                                                   TRUE,
                                                   FALSE ),
                  main = ifelse( isKeySet( LIST_arguments, "title" ),
                                 getValue( LIST_arguments, "title" ),
                                 NA ) )
}



if( STRING_function == "MDplot_clusters" )
{
  # check, if input is sane for this plot and get input files
  VEC_clustAll <- c( "clustersNumber",
                     "trajectoryNames" )
  testRequired( VEC_requiredForAll, LIST_arguments )
  testAllowed( c( VEC_clustAll,
                  VEC_allowedForAll ),
               LIST_arguments )
  if( isKeySet( LIST_arguments, "help" ) )
  {
    print_help( STRING_function,
                c( VEC_clustAll,
                   VEC_allowedForAll ),
                c( "<number of clusters (sorted by population) to be shown in the plot> (optional)",
                   "<vector of trajectory names, separated by ','> (optional)",
                   VEC_allowedForAllDesc ) )
    quit( save = "no", status = 0, runLast = TRUE )
  }
  VEC_files <- getFiles( getValue( LIST_arguments, "files" ) )
  for( i in 1:length( VEC_files ) )
    if( !file.exists( VEC_files[ i ] ) )
      stop( paste( "Error in file checking: seemingly, file",
                   VEC_files[ i ], "does not exist." ) )
  
  # load matrix and set names
  MAT_input <- MDplot_load_clusters( VEC_files )
  if( isKeySet( LIST_arguments, "trajectoryNames" ) )
  {
    VEC_names <- unlist( strsplit( getValue( LIST_arguments, "trajectoryNames" ),
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
                   clustersNumber = ifelse( isKeySet( LIST_arguments, "clustersNumber" ),
                                            getValue( LIST_arguments, "clustersNumber" ),
                                            NA ),
                   ownTrajectoryNames = ifelse( isKeySet( LIST_arguments,
                                                          "trajectoryNames" ),
                                                TRUE,
                                                FALSE ),
                   xlab = ifelse( is.na( VEC_axisNames ), "clusters", VEC_axisNames[ 1 ] ),
                   ylab = ifelse( is.na( VEC_axisNames ), "# configurations", VEC_axisNames[ 2 ] ),
                   main = ifelse( isKeySet( LIST_arguments, "title" ),
                                  getValue( LIST_arguments, "title" ),
                                  NA ) )
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
                                                VEC_names = NA )
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
                                             NA ) )
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
                               NA ) )
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
  VEC_acceptors = NA
  if( isKeySet( LIST_arguments, "acceptors" ) )
    VEC_acceptors <- as.numeric( unlist( strsplit( getValue( LIST_arguments, "acceptors" ),
                                                   ",",
                                                   fixed = TRUE ) ) )
  VEC_donors <- NA
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
                                          NA ) )
}
#########

# end
dev.off()
