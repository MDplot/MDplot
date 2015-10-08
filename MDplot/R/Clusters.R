# load function for "MDplot_clusters_timeseries"
MDplot_load_clusters_timeseries <- function( STRING_path, VEC_lengths, VEC_names = NA )
{
  
  # read input and get rid of column 2, which is unnecessary and pack everything in a list
  # of lists, where the first element is the name of the trajectory and the second is the
  # list of clusterIDs between the boundaries specified by the lengths of every trajectory
  TABLE_buf <- read.table( STRING_path )[ , -2 ]
  LIST_return <- list()
  INT_startLine <- 1  
  if( is.na( VEC_names ) ||
      length( VEC_lengths ) != length( VEC_names ) )
    VEC_names <- sapply( seq( 1, length( VEC_lengths ) ),
                         function( x ) paste( "trajectory",
                                              x,
                                              sep = "" ) )
  for( i in 1:length( VEC_lengths ) )
  {
    LIST_return[[ length( LIST_return ) + 1 ]] <- list( VEC_names[ i ], TABLE_buf[ INT_startLine:( INT_startLine + ( VEC_lengths[ i ] - 1 ) ),
                                                                               2 ] )
    INT_startLine <- INT_startLine + VEC_lengths[ i ]
  }
  #########
  return( LIST_return )
}

# plot timeseries of the clusters
MDplot_clusters_timeseries <- function( LIST_timeseries,
                                        INT_numberClusters = NA,
                                        VEC_selectTraj = NA,
                                        VEC_selectTime = NA,
                                        BOOL_printNanoseconds = FALSE,
                                        REAL_snapshotsPerNS = 1000,
                                        ... )
{
  
  # check all the input specified and select from data if necessary
  # in addition, generate the fake plotting matrix for the timeseries plot to generate the properly spanned area
  if( is.na( INT_numberClusters ) )
    INT_numberClusters <- max( unlist( lapply( LIST_timeseries, FUN = function( x ) unlist( x[[ 2 ]] ) ) ) )
  if( !is.na( VEC_selectTraj ) )
    LIST_timeseries <- LIST_timeseries[ VEC_selectTraj ] 
  if( !is.na( VEC_selectTime ) )
    for( i in 1:length( LIST_timeseries ) )
      LIST_timeseries[[ i ]][[ 2 ]] <- LIST_timeseries[[ i ]][[ 2 ]][ VEC_selectTime[ 1 ]:VEC_selectTime[ 2 ] ]
  INT_maxSnapshots <- max( unlist( lapply( LIST_timeseries, FUN = function( x ) length( unlist( x[[ 2 ]] ) ) ) ) )
  MAT_plotSpan <- matrix( nrow = length( LIST_timeseries ),
                          ncol = INT_maxSnapshots )
  #########
  
  # do the colors
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, "Spectral" ) )
  COLOURS_clusters <- PALETTE_colours( INT_numberClusters )
  #########

  # occurences and plot device division indeed
  par( mar = c( 2.0, 6.0, 7.5, 2.0 ) )
  REAL_widthOfOccurences <- ifelse( ( INT_numberClusters / 10 ) > 1.0,
                                    1.0,
                                    INT_numberClusters / 10 )
  layout( matrix( c( 1, 0, 2, 2 ), nrow = 2, byrow = TRUE ),
          widths = c( REAL_widthOfOccurences,
                      1.0 - REAL_widthOfOccurences,
                      1.0 ),
          heights = c( 1.5, 1.5, 1.5 ) )
  #########
  
  # calculate the percentages for the clusters
  VEC_occurences <- c()
  VEC_allClusterIDs <- unlist( lapply( LIST_timeseries, function( x ) x[[ 2 ]] ) )
  for( i in 1:INT_numberClusters )
    VEC_occurences <- c( VEC_occurences,
                         sum( VEC_allClusterIDs == i ) / length( VEC_allClusterIDs ) * 100 )
  #########
  
  # plot the top plot: all the occurences in percents
  PLOT_bp <- barplot( VEC_occurences,
                      xaxs = "i", yaxt = "n", xlab = "",
                      yaxs = "i", yaxt = "n", ylab = "populations",
                      bty = "n",
                      col = COLOURS_clusters )
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
  par( mar = c( 3.0, 6.0, 0.0, 2.0 ) )
  plot( MAT_plotSpan,
        xlim = c( 1, INT_maxSnapshots ),
        xaxs = "i", xaxt = "n", xlab = "",
        ylim = c( 0.575, length( LIST_timeseries ) + 0.425 ),
        yaxt = "n", ylab = "", yaxs = "i",
        bty = "n", type = "n" )
  mtext( side = 3, line = 14.0, cex = 1.45,
         text = ifelse( is.na( list( ... )[[ "main" ]] ),
                        "Cluster timeseries plot",
                        list( ... )[[ "main" ]] ) )
  axis( 1,
        at = split_equidistant( VEC_values = c( 0, INT_maxSnapshots ),
                                n = 5 ),
        labels = split_equidistant( VEC_values = c( 0, INT_maxSnapshots ),
                                    n = 5 ) /
                 ifelse( BOOL_printNanoseconds,
                         REAL_snapshotsPerNS,
                         1 ),
        tick = FALSE,
        line = -0.45 )
  axis( 2,
        at = 1:length( LIST_timeseries ),
        label = unlist( lapply( LIST_timeseries,
                                function( x ) x[[ 1 ]] ) ),
        tick = FALSE,
        las = 1 )
  mtext( side = 1, line = 1.75, cex = 1.0,
         text = paste( "time [",
                       ifelse( BOOL_printNanoseconds,
                               "ns",
                               "snapshots" ),
                       "]",
                       sep = "" ) )
  #########
  
  # plot the coloured lines representing the cluster occurences over time here
  if( length( LIST_timeseries ) > 1 )
    for( i in 1:length( LIST_timeseries ) )
    {
      VEC_clusterIDs <- unlist( LIST_timeseries[[ i ]][[ 2 ]] )
      VEC_xTicks <- seq( 1, length( LIST_timeseries[[ i ]][[ 2 ]] ) )
      for( j in 1:INT_numberClusters )
        segments( VEC_xTicks[ VEC_clusterIDs == j ],
                  rep( i - 0.425, length( VEC_xTicks[ VEC_clusterIDs == j ] ) ),
                  VEC_xTicks[ VEC_clusterIDs == j ],
                  rep( i + 0.425, length( VEC_xTicks[ VEC_clusterIDs == j ] ) ),
                  lwd = 0.65,
                  col = COLOURS_clusters[ j ] )
    }
  #########
}

# load function for "MDplot_clusters"
MDplot_load_clusters <- function( STRING_path )
{
  
  # load and transpose matrix
  MAT_pre <- as.matrix( read.table( STRING_path ) )[ , -1  ]
  MAT_pre <- MAT_pre[ , ( ( ncol( MAT_pre ) / 2 ) + 1 ):ncol( MAT_pre ) ]
  MAT_pre <- t( MAT_pre )
  #########
  return( MAT_pre )
}

# plot the clusters
MDplot_clusters <- function( MAT_clusters,
                             INT_numberClusters = NA,
                             STRING_legend_title = "trajectories",
                             BOOL_ownTrajectoryNames = FALSE,
                             ... )
{
  # reduce number of clusters, in case specified and take care of the trajectory names
  if( !is.na( INT_numberClusters ) )
    MAT_clusters <- MAT_clusters[ , 1:INT_numberClusters ]
  colnames( MAT_clusters ) <- 1:ncol( MAT_clusters )
  if( !BOOL_ownTrajectoryNames )
    rownames( MAT_clusters ) <- 1:nrow( MAT_clusters )
  #########
  
  PALETTE_clusters <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_CLUSTERS <- PALETTE_clusters( nrow( MAT_clusters ) )
  names( MAT_clusters ) <- 1:nrow( MAT_clusters )
  barplot( MAT_clusters, col = COLOURS_CLUSTERS, ... )
  legend( "topright", inset = 0.045, legend = rownames( MAT_clusters ),
          title = STRING_legend_title, box.lty = 0, box.lwd = 0, 
          col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
  #########
}