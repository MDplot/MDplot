# load the XRMSD data
# WARNING: very sensitive to proper file format (line skipping and end ignoring)
load_xrmsd <- function( STRING_path,
                        INT_skipBeginning = 8,
                        INT_factor = 10000 )
{
  
  # get total line number and subtract end and header after header skipping
  InputFile <- readLines( STRING_path )
  MAT_return <- as.matrix( read.table( STRING_path,
                                       skip = INT_skipBeginning,
                                       nrows = length( InputFile ) -
                                               ( INT_skipBeginning + 2 ) ) )
  #########
  
  # divide RMSD integer values by the proper factor (usually 10000) and return resulting matrix
  MAT_return[ , 3 ] <- MAT_return[ , 3 ] / INT_factor
  return( MAT_return )  
  #########
}

# do 2D XRMSD heatmap plot, with possible legend
# TODO: allow user to set appropriate colour span
xrmsd <- function( MAT_values,
                   BOOL_printLegend = TRUE,
                   VEC_xAxisRange = NA,
                   VEC_yAxisRange = NA,
                   VEC_colourRange = NA,
                   ... )
{
  
  # check user supplied input and replace in case undefined
  if( is.na( VEC_xAxisRange ) )
    VEC_xAxisRange <- c( min( MAT_values[ , 1 ] ),
                          max( MAT_values[ , 1 ] ) )
  if( is.na( VEC_yAxisRange ) )
    VEC_yAxisRange <- c( min( MAT_values[ , 2 ] ),
                          max( MAT_values[ , 2 ] ) )
  VEC_colourRange <- c( 0, 
                         max( MAT_values[ , 3 ] ) )
  #########
  
  # colour values accordingly
  if( BOOL_printLegend )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
  PALETTE_colours_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_coloursPlot <- PALETTE_colours_rev( 11 )[ as.numeric( cut( as.numeric( MAT_values[ , 3 ] ), breaks = 10 ) ) ]
  #########
  
  # plot the heatmap thing
  plot( MAT_values[ , 1:2 ],
        col = VEC_coloursPlot,
        bg = VEC_coloursPlot,
        pch = 22,
        cex = 2.75 / log( ( ( VEC_xAxisRange[ 2 ] - VEC_xAxisRange[ 1 ] ) *
                            ( VEC_yAxisRange[ 2 ] - VEC_yAxisRange[ 1 ] ) ) ),
        xaxs = "i", yaxs = "i",
        xlim = VEC_xAxisRange, ylim = VEC_yAxisRange,
        ... )
  #########
  
  # print legend in case it is specified
  if( BOOL_printLegend )
  {
    legend_image <- as.raster( matrix( PALETTE_colours( 11 ), ncol = 1 ) )
    plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = 'Legend [nm]' )
    text( x = 1.5, y = seq( 0, 1, l = 5 ),
          labels = round( seq( VEC_colourRange[ 1 ], VEC_colourRange[ 2 ], l = 5 ), digits = 2 ) )
    rasterImage( legend_image, 0, 0, 1, 1 )
  }
  #########
}