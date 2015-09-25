# load function for "MDplot_clusters"
MDplot_load_clusters <- function( STRING_path )
{
  MAT_pre <- as.matrix( read.table( STRING_path ) )[ , -1  ]
  MAT_pre <- MAT_pre[ , ( ( ncol( MAT_pre ) / 2 ) + 1 ):ncol( MAT_pre ) ]
  return( MAT_pre )
}

# print the clusters
MDplot_clusters <- function( MAT_clusters,
                             INT_numberClusters = NULL,
                             STRING_legend_title = "trajectories",
                             BOOL_trajectory_names = FALSE,
                             ... )
{
  if( !is.null( INT_numberClusters ) )
    MAT_clusters <- MAT_clusters[ 1:INT_numberClusters, ]
  
  # transpose cluster's matrix and plot it
  MAT_clusters <- t( MAT_clusters )
  if( is.null( colnames( MAT_clusters ) ) )
  {
    colnames( MAT_clusters ) <- 1:ncol( MAT_clusters )
  }
  if( !BOOL_trajectory_names )
  {
    rownames( MAT_clusters ) <- 1:nrow( MAT_clusters )
  }
  PALETTE_clusters <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_CLUSTERS <- PALETTE_clusters( nrow( MAT_clusters ) )
  names( MAT_clusters ) <- 1:nrow( MAT_clusters )
  barplot( MAT_clusters, col = COLOURS_CLUSTERS, ... )
  legend( "topright", inset = 0.045, legend = rownames( MAT_clusters ),
          title = STRING_legend_title, box.lwd = 0, 
          col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
  #########
}