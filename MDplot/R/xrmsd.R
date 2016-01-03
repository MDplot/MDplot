# load the XRMSD data
# WARNING: very sensitive to proper file format (line skipping and end ignoring)
load_xrmsd <- function( path,
                        skipBeginning = 8,
                        factor = 10000 )
{
  
  # get total line number and subtract end and header after header skipping
  InputFile <- readLines( path )
  MAT_return <- as.matrix( read.table( path,
                                       skip = skipBeginning,
                                       nrows = length( InputFile ) -
                                               ( skipBeginning + 2 ) ) )
  #########
  
  # divide RMSD integer values by the proper factor (usually 10000) and return resulting matrix
  MAT_return[ , 3 ] <- MAT_return[ , 3 ] / factor
  return( MAT_return )  
  #########
}

# do 2D XRMSD heatmap plot, with possible legend
# TODO: allow user to set appropriate colour span
xrmsd <- function( xrmsdValues,
                   printLegend = TRUE,
                   xaxisRange = NA,
                   yaxisRange = NA,
                   colours = NA,
                   ... )
{
  
  # check user supplied input and replace in case undefined
  if( is.na( xaxisRange ) )
    xaxisRange <- c( min( xrmsdValues[ , 1 ] ),
                          max( xrmsdValues[ , 1 ] ) )
  if( is.na( yaxisRange ) )
    yaxisRange <- c( min( xrmsdValues[ , 2 ] ),
                     max( xrmsdValues[ , 2 ] ) )
  colours <- c( 0, 
                max( xrmsdValues[ , 3 ] ) )
  #########
  
  # colour values accordingly
  par( mar = c( 2.5, 2.5, 2.5, 0.5 ) )
  if( printLegend )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
  PALETTE_colours_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_coloursPlot <- PALETTE_colours_rev( 11 )[ as.numeric( cut( as.numeric( xrmsdValues[ , 3 ] ), breaks = 10 ) ) ]
  #########
  
  # plot the heatmap thing
  plot( xrmsdValues[ , 1:2 ],
        col = VEC_coloursPlot,
        bg = VEC_coloursPlot,
        pch = 22,
        cex = 2.75 / log( ( ( xaxisRange[ 2 ] - xaxisRange[ 1 ] ) *
                            ( yaxisRange[ 2 ] - yaxisRange[ 1 ] ) ) ),
        xaxs = "i", yaxs = "i",
        xlim = xaxisRange, ylim = yaxisRange,
        ... )
  #########
  
  # print legend in case it is specified
  if( printLegend )
  {
    legend_image <- as.raster( matrix( PALETTE_colours( 11 ), ncol = 1 ) )
    plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = 'Legend [nm]' )
    text( x = 1.5, y = seq( 0, 1, l = 5 ),
          labels = round( seq( colours[ 1 ], colours[ 2 ], l = 5 ), digits = 2 ) )
    rasterImage( legend_image, 0, 0, 1, 1 )
  }
  #########
}