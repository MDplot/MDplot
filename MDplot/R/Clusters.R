# load function for "MDplot_clusters"
MDplot_load_clusters <- function( STRING_path )
{
  MAT_pre <- as.matrix( read.table( STRING_path ) )[ , -1  ]
  MAT_pre <- MAT_pre[ , ( ( ncol( MAT_pre ) / 2 ) + 1 ):ncol( MAT_pre ) ]
  return( MAT_pre )
}

# print the clusters
MDplot_clusters <- function( MAT_clusters, INT_maximum_number = 0, STRING_legend_title = "trajectories", 
                             ylab = "# of cluster members", xlab = "# cluster", main = "cluster plot", 
                             ... )
{
  if( INT_maximum_number != 0 )
  {
    MAT_clusters <- MAT_clusters[ 1:INT_maximum_number, ]
  }
  
  # transpose cluster's matrix and plot it
  MAT_clusters <- t( MAT_clusters )
  colnames( MAT_clusters ) <- 1:ncol( MAT_clusters )
  PALETTE_clusters <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_CLUSTERS <- PALETTE_clusters( nrow( MAT_clusters ) )
  names( MAT_clusters ) <- 1:nrow( MAT_clusters )
  barplot( MAT_clusters, col = COLOURS_CLUSTERS, ylab = ylab, xlab = xlab, main = main, ... )
  legend( "topright", inset = 0.045, legend = 1:nrow( MAT_clusters ), title = STRING_legend_title, box.lwd = 0, col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
  #########
}