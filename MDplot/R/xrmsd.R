# load the XRMSD data
# NOTE: when testing 'GROMACS' support, also use additional input
load_xrmsd <- function( path,
                        factor = 10000,
                        removeLowerHalf = TRUE,
                        mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" &&
      mdEngine != "AMBER" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )

  if( mdEngine == "GROMOS" )
  {
    # get total line number and subtract end and header after header skipping
    INT_skipBeginning <- NA
    INT_skipBeginning <- 8
    InputFile <- readLines( path )
    MAT_return <- as.matrix( read.table( path,
                                         skip = INT_skipBeginning,
                                         nrows = length( InputFile ) -
                                                 ( INT_skipBeginning + 2 ) ) )
  
    # divide RMSD integer values by the proper factor (usually 10000) and return resulting matrix
    MAT_return[ , 3 ] <- MAT_return[ , 3 ] / factor
    return( MAT_return )  
  }
  if( mdEngine == "GROMACS" )
  {
    XPM_data <- load_XPM( path )
    MAT_return <- matrix( c( rep( 1:XPM_data$numberRows, each = XPM_data$numberColumns ),
                             rep( 1:XPM_data$numberColumns, times = XPM_data$numberRows ),
                             rep( NA, times = XPM_data$numberRows * XPM_data$numberColumns ) ),
                          ncol = 3,
                          byrow = FALSE )
    for( i in 1:nrow( MAT_return ) )
      MAT_return[ i, 3 ] <- XPM_data$colorComments[ match( XPM_data$data[ MAT_return[  i, 1 ], MAT_return[ i, 2 ]  ],
                                                           XPM_data$usedChars ) ]
    MAT_return[ , 2 ] <- rev( MAT_return[ , 2 ] )
    if( removeLowerHalf == FALSE ) 
      return( MAT_return )
    else
      return( MAT_return[ MAT_return[ , 1 ] <= MAT_return[ , 2 ], ] )
  }
  if( mdEngine == "AMBER" )
  {
    TABLE_input <- read.table( path )[ , -1, drop = FALSE ]
    MAT_return <- matrix( c( rep( 1:nrow( TABLE_input ), each = ncol( TABLE_input ) ),
                             rep( 1:ncol( TABLE_input ), times = nrow( TABLE_input ) ),
                             rep( NA, times = ncol( TABLE_input ) * nrow( TABLE_input ) ) ),
                          ncol = 3,
                          byrow = FALSE )
    for( i in 1:nrow( MAT_return ) )
      MAT_return[ i, 3 ] <- TABLE_input[ MAT_return[ i, 1 ],
                                         MAT_return[ i, 2 ] ]
    if( removeLowerHalf == FALSE ) 
      return( MAT_return )
    else
      return( MAT_return[ MAT_return[ , 1 ] <= MAT_return[ , 2 ], ] )
  }
}

# do 2D XRMSD heatmap plot, with possible legend
# TODO: allow user to set appropriate colour span
xrmsd <- function( xrmsdValues,
                   printLegend = TRUE,
                   xaxisRange = NA,
                   yaxisRange = NA,
                   colours = NA,
                   rmsdUnit = "nm",
                   barePlot = FALSE,
                   ... )
{
  
  # check user supplied input and replace in case undefined
  if( all( is.na( xaxisRange ) ) )
    xaxisRange <- c( min( xrmsdValues[ , 1 ] ),
                     max( xrmsdValues[ , 1 ] ) )
  if( all( is.na( yaxisRange ) ) )
    yaxisRange <- c( min( xrmsdValues[ , 2 ] ),
                     max( xrmsdValues[ , 2 ] ) )
  colours <- c( 0, 
                max( xrmsdValues[ , 3 ] ) )
  #########

  defaultArguments <- list( xlab = ifelse( barePlot,
                                           "",
                                           "conformation" ),
                            ylab = ifelse( barePlot,
                                           "",
                                           "conformation" ),
                            main = "" )
  ellipsis <- list( ... )
  defaultArguments[ names( ellipsis ) ] <- ellipsis
  ellipsis[ names( defaultArguments ) ] <- defaultArguments
  
  # colour values accordingly
  if( printLegend )
    layout( matrix( 1:2, ncol = 2 ), widths = c( 2, 1 ), heights = c( 1, 1 ) )
  PALETTE_colours <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
  PALETTE_colours_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_coloursPlot <- PALETTE_colours_rev( 11 )[ as.numeric( cut( as.numeric( xrmsdValues[ , 3 ] ), breaks = 10 ) ) ]
  #########
  
  # plot the heatmap thing
  do.call( what = plot,
           c( list( xrmsdValues[ , 1:2 ],
                    col = VEC_coloursPlot,
                    bg = VEC_coloursPlot,
                    pch = 22,
                    cex = 2.75 / log( ( ( xaxisRange[ 2 ] - xaxisRange[ 1 ] ) *
                                        ( yaxisRange[ 2 ] - yaxisRange[ 1 ] ) ) ),
                    xaxs = "i", yaxs = "i",
                    xlim = xaxisRange, ylim = yaxisRange ),
                    ellipsis ) )
  #########
  
  # print legend in case it is specified
  if( printLegend )
  {
    legend_image <- as.raster( matrix( PALETTE_colours( 11 ), ncol = 1 ) )
    plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = paste( 'Legend [',
                                                                                           rmsdUnit,
                                                                                           ']',
                                                                                           sep = "" ) )
    text( x = 1.5, y = seq( 0, 1, l = 5 ),
          labels = round( seq( colours[ 1 ], colours[ 2 ], l = 5 ), digits = 2 ) )
    rasterImage( legend_image, 0, 0, 1, 1 )
  }
  #########
}