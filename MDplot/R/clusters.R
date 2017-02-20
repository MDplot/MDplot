# loading function for "clusters_ts()"
load_clusters_ts <- function( path,
                              lengths,
                              names = NA,
                              mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" &&
      mdEngine != "AMBER" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )

  LIST_return <- list()
  if( all( is.na( names ) ) ||
      length( lengths ) != length( names ) )
    names <- sapply( seq( 1, length( lengths ) ),
                     function( x ) paste( "trajectory",
                                          x,
                                          sep = "" ) )

  if( mdEngine == "GROMOS" )
  {
    # read input and get rid of column 2, which is unnecessary and pack everything in a list
    # of lists, where the first element is the name of the trajectory and the second is the
    # list of clusterIDs between the boundaries specified by the lengths of every trajectory
    TABLE_buf <- read.table( path )[ , -2 ]
    INT_startLine <- 1  
    for( i in 1:length( lengths ) )
    {
      LIST_return[[ length( LIST_return ) + 1 ]] <- list( names[ i ], TABLE_buf[ INT_startLine:( INT_startLine + ( lengths[ i ] - 1 ) ),
                                                                                 2 ] )
      INT_startLine <- INT_startLine + lengths[ i ]
    }
  }
  if( mdEngine == "GROMACS" )
  {
    LIST_temp <- load_clusters_GROMACS( path, lengths )
    for( i in 1:length( LIST_temp ) )
      LIST_return[[ length( LIST_return ) + 1 ]] <- list( names[ i ], unlist( LIST_temp[[ i ]][ , 2 ] ) )
  }
  if( mdEngine == "AMBER" )
  {
    TABLE_buf <- read.table( path )
    INT_startLine <- 1  
    for( i in 1:length( lengths ) )
    {
      TABLE_traj <- TABLE_buf[ INT_startLine:( INT_startLine + ( lengths[ i ] - 1 ) ),
                               2 ]
      TABLE_traj[ TABLE_traj == -1 ] <- 0
      LIST_return[[ length( LIST_return ) + 1 ]] <- list( names[ i ],
                                                          TABLE_traj )
      INT_startLine <- INT_startLine + lengths[ i ]
    }
  }
  return( LIST_return )
}

# plot timeseries of the clusters
clusters_ts <- function( clustersDataTS,
                         clustersNumber = NA,
                         selectTraj = NA,
                         selectTime = NA,
                         timeUnit = NA,
                         snapshotsPerTimeInt = 1000,
                         ... )
{
  
  # check all the input specified and select from data if necessary
  # in addition, generate the fake plotting matrix for the timeseries plot to generate the properly spanned area
  if( is.na( clustersNumber ) )
    clustersNumber <- max( unlist( lapply( clustersDataTS, FUN = function( x ) unlist( x[[ 2 ]] ) ) ) )
  if( all( !is.na( selectTraj ) ) )
    clustersDataTS <- clustersDataTS[ selectTraj ] 
  if( all( !is.na( selectTime ) ) )
    for( i in 1:length( clustersDataTS ) )
      clustersDataTS[[ i ]][[ 2 ]] <- clustersDataTS[[ i ]][[ 2 ]][ selectTime[ 1 ]:selectTime[ 2 ] ]
  INT_maxSnapshots <- max( unlist( lapply( clustersDataTS, FUN = function( x ) length( unlist( x[[ 2 ]] ) ) ) ) )
  MAT_plotSpan <- matrix( nrow = length( clustersDataTS ),
                          ncol = INT_maxSnapshots )
  #########
  
  # do the colors
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, "Spectral" ) )
  COLOURS_clusters <- PALETTE_colours( clustersNumber )
  #########

  # occurences and plot device division indeed
  par( mar = c( 3.0, 7.0, 4.5, 2.0 ) )
  REAL_widthOfOccurences <- ifelse( ( clustersNumber / 10 ) > 1.0,
                                    1.0,
                                    clustersNumber / 10 )
  if( !is.null( list( ... )[[ "main"]] ) )
  {
    layout( matrix( c( 1, 1, 2, 0, 3, 3 ), nrow = 3, byrow = TRUE ),
            widths = c( 1.0,
                        1 - REAL_widthOfOccurences,
                        1.0 ),
            heights = c( 0.4, 1.0, 1.5 ) )
    plot.new()
    mtext( side = 3, padj = 0, cex = 1.45, text = list( ... )[[ "main" ]] )
  }
  else
  {
    layout( matrix( c( 1, 0, 2, 2 ), nrow = 2, byrow = TRUE ),
            widths = c( REAL_widthOfOccurences,
                        1.0 - REAL_widthOfOccurences,
                        1.0 ),
            heights = c( 1.5, 1.5, 1.5 ) )
  }
  #########
  
  # calculate the percentages for the clusters
  VEC_occurences <- c()
  VEC_allClusterIDs <- unlist( lapply( clustersDataTS, function( x ) x[[ 2 ]] ) )
  for( i in 1:clustersNumber )
    VEC_occurences <- c( VEC_occurences,
                         sum( VEC_allClusterIDs == i ) / length( VEC_allClusterIDs ) * 100 )
  MAT_printResults <- matrix( setNumberDigits( VEC_occurences,
                                               2 ),
                              ncol = length( VEC_occurences ) )
  VEC_colNames <- c()
  for( i in 1:length( VEC_occurences ) )
    VEC_colNames <- c( VEC_colNames,
                       paste( "cluster",
                              i,
                              sep = "" ) )
  colnames( MAT_printResults ) <- VEC_colNames
  #########
  
  # plot the top plot: all the occurences in percents
  PLOT_bp <- barplot( VEC_occurences,
                      xaxs = "i", yaxt = "n", xlab = "",
                      yaxs = "i", yaxt = "n", ylab = "populations",
                      bty = "n",
                      col = COLOURS_clusters, cex.lab = 1.45 )
  mtext( sapply( VEC_occurences,
                 FUN = function( x ) paste( round( x, digits = 1 ),
                                            "%" ), 
                 simplify = TRUE ),
         side = 3,
         las = 3,
         at = PLOT_bp,
         line = 0.45 )
  axis( 1,
        labels = 1:length( VEC_occurences ),
        at = PLOT_bp,
        tick = FALSE,
        line = -0.45 )
  #########
  
  # reset margins
  # plot the bottom plot: set the framework for the plotting later on
  par( mar = c( 4.0, 7.0, 0.0, 2.0 ) )
  plot( MAT_plotSpan,
        xlim = c( 1, INT_maxSnapshots ),
        xaxs = "i", xaxt = "n", xlab = "",
        ylim = c( 0.575, length( clustersDataTS ) + 0.425 ),
        yaxt = "n", ylab = "", yaxs = "i",
        bty = "n", type = "n" )
  axis( 1,
        at = split_equidistant( VEC_values = c( 0, INT_maxSnapshots ),
                                n = 5 ),
        labels = split_equidistant( VEC_values = c( 0, INT_maxSnapshots ),
                                    n = 5 ) /
                 ifelse( !is.na( timeUnit ),
                         snapshotsPerTimeInt,
                         1 ),
        tick = FALSE,
        line = -0.45,
        cex.axis = 1.25 )
  axis( 2,
        at = 1:length( clustersDataTS ),
        labels = unlist( lapply( clustersDataTS,
                                function( x ) x[[ 1 ]] ) ),
        tick = FALSE,
        las = 1,
        cex.axis = 1.25 )
  mtext( side = 1, line = 2.25, cex = 1,
         text = paste( "time [",
                       ifelse( is.na( timeUnit ),
                               "snapshots",
                               timeUnit ),
                       "]",
                       sep = "" ) )
  #########
  
  # plot the coloured lines representing the cluster occurences over time here
  for( i in 1:length( clustersDataTS ) )
  {
    VEC_trajOccurences <- c()
    VEC_clusterIDs <- unlist( clustersDataTS[[ i ]][[ 2 ]] )
    VEC_xTicks <- seq( 1, length( clustersDataTS[[ i ]][[ 2 ]] ) )
    for( j in 1:clustersNumber )
    {
      segments( VEC_xTicks[ VEC_clusterIDs == j ],
                rep( i - 0.425, length( VEC_xTicks[ VEC_clusterIDs == j ] ) ),
                VEC_xTicks[ VEC_clusterIDs == j ],
                rep( i + 0.425, length( VEC_xTicks[ VEC_clusterIDs == j ] ) ),
                lwd = 0.65,
                col = COLOURS_clusters[ j ] )
      VEC_trajOccurences <- c( VEC_trajOccurences,
                               length( VEC_clusterIDs[ VEC_clusterIDs == j ] ) / length( VEC_clusterIDs ) * 100 )
    }
    MAT_printResults <- rbind( MAT_printResults,
                               setNumberDigits( VEC_trajOccurences,
                                                2 ) )
  }
  rownames( MAT_printResults ) <- c( "overall",
                                     unlist( lapply( clustersDataTS, function( x ) x[[ 1 ]] ) ) )
  #########
  
  return( MAT_printResults )
}

# load input for GROMACS
load_clusters_GROMACS <- function( path,
                                   lengths )
{
  inputData <- readLines( path )
  VEC_times <- c()
  VEC_clusters <- c()
  for( i in 1:length( inputData ) )
  {
    if( substr( inputData[ i ], 1, 5 ) == "@TYPE" )
    {
      for( j in ( i + 1 ):length( inputData ) )
      {
        if( substr( inputData[ j ], 1, 1 ) != "@" )
        {
          for( k in j:length( inputData ) )
          {
            VEC_curLine <- unlist( strsplit( inputData[ k ], " +" ) )
            VEC_clusters <- c( VEC_clusters, as.numeric( VEC_curLine[ 3 ] ) )
            VEC_times <- c( VEC_times, as.numeric( VEC_curLine[ 2 ] ) )
          }
          MAT_input <- matrix( c( VEC_times,
                                  VEC_clusters ),
                                  ncol = 2,
                               byrow = FALSE )
          LIST_temp <- NULL
          if( all( !is.na( lengths ) ) )
          {
            if( sum( lengths ) != nrow( MAT_input ) )
              stop( "The sum of lengths must be the same as the number of lines in the input!" )
            INT_lastBegin <- 1
            for( k in 1:length( lengths ) )
            {
              LIST_temp[[ length( LIST_temp ) + 1 ]] <- MAT_input[ INT_lastBegin:( INT_lastBegin +
                                                                                   lengths[ k ] -
                                                                                   1 ),
                                                                   ,
                                                                   drop = FALSE ]
              INT_lastBegin <- lengths[ k ] + 1
            }
          }
          else
            LIST_temp <- list( MAT_input )
          return( LIST_temp )
        }
      }
    }
  }
}

# loading function for "clusters()"
load_clusters <- function( path,
                           names = NA,
                           lengths = NA,
                           mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" &&
      mdEngine != "AMBER" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )
  MAT_pre <- NULL
  if( mdEngine == "GROMOS" )
  {
    # load and transpose matrix
    MAT_pre <- as.matrix( read.table( path ) )[ , -1  ]
    MAT_pre <- MAT_pre[ , ( ( ncol( MAT_pre ) / 2 ) + 1 ):ncol( MAT_pre ) ]
    MAT_pre <- t( MAT_pre )
  }
  if( mdEngine == "GROMACS" )
  {
    LIST_temp <- load_clusters_GROMACS( path, lengths )
    INT_numberClusters <- max( unlist( lapply( LIST_temp,
                                               FUN = function( x ) max( unlist( x[ , 2 ] ) ) ) ) )
    MAT_pre <- matrix( rep( 0, times = INT_numberClusters * length( LIST_temp ) ),
                       nrow = length( LIST_temp ) )
    for( i in 1:length( LIST_temp ) )
      for( j in 1:INT_numberClusters )
        MAT_pre[ i, j ] <- sum( LIST_temp[[ i ]][ , 2 ] == j )
  }
  if( mdEngine == "AMBER" )
  {
    # read comment line and the input table
    CON_input <- file( path, open = "r" )
    VEC_commentLine <- NA
    while( length( STRING_theLine <- readLines( CON_input, n = 1, warn = FALSE ) ) > 0 )
    {
      VEC_commentLine <- unlist( strsplit( STRING_theLine, split = " +" ) )
      if( VEC_commentLine[ 1 ] == "#Cluster" )
        break
    }
    close( CON_input )
    TABLE_input <- read.table( path )

    # depending on input, generate output matrix and decide on processing
    MAT_pre <- matrix( rep( 0, times = nrow( TABLE_input ) ),
                       nrow = 1 )
    if( "NumIn1st" %in% VEC_commentLine )
    {
      # more than one trajectories
      VEC_indices <- grep( "NumIn", VEC_commentLine )
      for( i in 1:length( VEC_indices ) )
        MAT_pre <- rbind( MAT_pre,
                          TABLE_input[ , VEC_indices[ i ] ] )
      MAT_pre <- MAT_pre[ -1, ]
    }
    else
    {
      # only one trajectory
      MAT_pre[ 1, ] <- TABLE_input[ , 2 ]
    }
  }
  if( all( !is.na( names ) ) &&
      length( names ) == nrow( MAT_pre ) )
  {
    rownames( MAT_pre ) <- names
  }
  else
  {
    rownames( MAT_pre ) <- 1:nrow( MAT_pre )
  }
  #########
  return( MAT_pre )
}

# plot the clusters
clusters <- function( clusters,
                      clustersNumber = NA,
                      legendTitle = "trajectories",
                      barePlot = FALSE,
                      ... )
{
  # reduce number of clusters, in case specified and take care of the trajectory names
  if( !is.na( clustersNumber ) )
    clusters <- clusters[ , 1:clustersNumber, drop = FALSE ]
  colnames( clusters ) <- 1:ncol( clusters )
  #########
  
  # plot clusters
  PALETTE_clusters <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_CLUSTERS <- PALETTE_clusters( nrow( clusters ) )
  defaultArguments <- list( xlab = ifelse( barePlot, "", "clusters" ),
                            main = "",
                            col = COLOURS_CLUSTERS )
  ellipsis <- list( ... )
  defaultArguments[ names( ellipsis ) ] <- ellipsis
  ellipsis[ names( defaultArguments ) ] <- defaultArguments
  do.call( what = barplot,
           c( list( height = clusters,
                    xaxt = ifelse( barePlot, "n", "s" ),
                    yaxt = ifelse( barePlot, "n", "s" ) ),
              ellipsis ) )
  if( !barePlot )
    legend( "topright", inset = 0.045, legend = rownames( clusters ),
            title = legendTitle, box.lty = 0, box.lwd = 0, 
            col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
  #########
  
  VEC_clusterNames <- c()
  for( i in 1:ncol( clusters ) )
    VEC_clusterNames <- c( VEC_clusterNames,
                           paste( "cluster",
                                  i,
                                  sep = "" ) )
  colnames( clusters ) <- VEC_clusterNames
  return( clusters )
}