# load the XRMSD data
# WARNING: very sensitive to proper file format (line skipping and end ignoring)
MDplot_load_XRMSD <- function( STRING_path, INT_skip_beginning = 8, INT_factor = 10000 )
{
  
  # get total line number and subtract end and header after header skipping
  InputFile <- readLines( STRING_path )
  MAT_return <- as.matrix( read.table( STRING_path,
                                       skip = INT_skip_beginning,
                                       nrows = length( InputFile ) -
                                               ( INT_skip_beginning + 2 ) ) )
  #########
  
  # divide RMSD integer values by the proper factor (usually 10000) and return resulting matrix
  MAT_return[ , 3 ] <- MAT_return[ , 3 ] / INT_factor
  return( MAT_return )  
  #########
}

# do 2D XRMSD heatmap plot, with possible legend
# TODO: allow user to set appropriate colour span
MDplot_XRMSD <- function( MAT_values,
                          BOOL_printLegend = TRUE,
                          VEC_xaxis_range = NULL,
                          VEC_yaxis_range = NULL,
                          VEC_colour_range = NULL )
{
  
  # check user supplied input and replace in case undefined
  if( is.null( VEC_xaxis_range ) )
    VEC_xaxis_range <- c( min( MAT_values[ , 1 ] ),
                          max( MAT_values[ , 1 ] ) )
  if( is.null( VEC_yaxis_range ) )
    VEC_yaxis_range <- c( min( MAT_values[ , 2 ] ),
                          max( MAT_values[ , 2 ] ) )
  VEC_colour_range <- c( 0, 
                         max( MAT_values[ , 3 ] ) )
  #########
  
  # colour values accordingly
  if( BOOL_printLegend )
  {
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
  }
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
  PALETTE_colours_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_coloursPlot <- PALETTE_colours_rev( 11 )[ as.numeric( cut( as.numeric( MAT_values[ , 3 ] ), breaks = 10 ) ) ]
  #########
  
  # plot the heatmap thing
  plot( MAT_values[ , 1:2 ],
        col = VEC_coloursPlot,
        bg = VEC_coloursPlot,
        pch = 22,
        cex = 2.75 / log( ( ( VEC_xaxis_range[ 2 ] - VEC_xaxis_range[ 1 ] ) *
                            ( VEC_yaxis_range[ 2 ] - VEC_yaxis_range[ 1 ] ) ) ),
        xaxs = "i", yaxs = "i",
        xlim = VEC_xaxis_range, ylim = VEC_yaxis_range )
  #########
  
  # print legend in case it is specified
  if( BOOL_printLegend )
  {
    legend_image <- as.raster( matrix( PALETTE_colours( 11 ), ncol = 1 ) )
    plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = 'Legend [nm]' )
    text( x = 1.5, y = seq( 0, 1, l = 5 ),
          labels = round( seq( VEC_colour_range[ 1 ], VEC_colour_range[ 2 ], l = 5 ), digits = 2 ) )
    rasterImage( legend_image, 0, 0, 1, 1 )
  }
  #########
}