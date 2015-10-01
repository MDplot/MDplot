# load function for "MDplot_clusters_timeseries"
MDplot_load_clusters_timeseries <- function( STRING_path, VEC_lengths, VEC_names = NULL )
{
  TABLE_buf <- read.table( STRING_path )[ , -2 ]
  LIST_return <- list()
  INT_startLine <- 1  
  if( is.null( VEC_names ) ||
      length( VEC_lengths ) != length( VEC_names ) )
    VEC_names <- seq( 1, length( VEC_lengths ) )
  for( i in 1:length( VEC_lengths ) )
  {
    LIST_return[[ length( LIST_return ) + 1 ]] <- list( VEC_names[i], TABLE_buf[ INT_startLine:( INT_startLine + ( VEC_lengths[ i ] - 1 ) ),
                                                                               2 ] )
    INT_startLine <- INT_startLine + VEC_lengths[ i ]
  }
  return( LIST_return )
}

# plot timeseries of the clusters
MDplot_clusters_timeseries <- function( LIST_timeseries,
                                        INT_numberClusters = NULL )
{
  INT_maxSnapshots <- max( unlist( lapply( LIST_timeseries, FUN = function( x ) length( unlist( x[[ 2 ]] ) ) ) ) )
  if( is.null( INT_numberClusters ) )
    INT_numberClusters <- max( unlist( lapply( LIST_timeseries, FUN = function( x ) unlist( x[[ 2 ]] ) ) ) )
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, "Spectral" ) )
  COLOURS_clusters <- PALETTE_colours( INT_numberClusters )
  MAT_plotSpan <- matrix( nrow = length( LIST_timeseries ),
                          ncol = INT_maxSnapshots )
  plot( MAT_plotSpan,
        xlim = c( 1, INT_maxSnapshots ),
        xaxs = "i", xaxt = "n", xlab = "",
        ylim = c( 0.575, length( LIST_timeseries ) + 0.425 ),
        yaxt = "n", ylab = "", yaxs = "i",
        type = "n" )
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
                  lwd = 1.0,
                  col = COLOURS_clusters[ j ] )
    }
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
                             INT_numberClusters = NULL,
                             STRING_legend_title = "trajectories",
                             BOOL_ownTrajectoryNames = FALSE,
                             ... )
{
  # reduce number of clusters, in case specified and take care of the trajectory names
  if( !is.null( INT_numberClusters ) )
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
          title = STRING_legend_title, box.lwd = 0, 
          col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
  #########
}