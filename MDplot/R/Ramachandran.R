# load ramachandran data into a matrix, select columns
MDplot_load_ramachandran <- function( STRING_file,
                                      VEC_columns = c( 3, 4 ) )
{
  
  # load and parse matrix, return result
  MAT_input <- as.matrix( read.table( STRING_file ) )
  if( ncol( MAT_input ) < max( VEC_columns ) )
    stop( paste( "Error while loading and parsing file '", STRING_file, "' since the number ",
                 "of columns is less than the maximum column number specified." ) )
  return( MAT_input[ , VEC_columns ] )
}


# plot the angles on a x = ( -180, 180 ) to y = ( -180, 180 ) area
MDplot_ramachandran <- function( MAT_dihedrals,
                                 xbins = 150,
                                 ybins = 150,
                                 heatFun = "log", 
                                 LIST_areas = c(),
                                 STRING_plotType = "sparse",
                                 ... )
{
  
  # settings (small offset for label printing required)
  VEC_xTicks  <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  VEC_xLabels <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  PALETTE_histogramColours <- colorRampPalette( c( "white",
                                                   rev( brewer.pal( 11, 'Spectral' ) ) ) )
  #########
  
  # determine function for plotting (logarithm is useful for lots of points)
  FUN_heatFun = function( x ) log( x )
  if( heatFun == "log" )
  {}
  else if( heatFun == "norm" )
    FUN_heatFun = function( x ) x
  else
    stop( paste( "Error: the function '", heatFun, "' for parameter 'heatFun' is not defined.", sep = "" ) )
  #########
  
  if( STRING_plotType == "sparse" )
    hist2d( MAT_dihedrals, nbins = c( xbins, ybins ), same.scale = FALSE, na.rm = TRUE, 
            show = TRUE, col = PALETTE_histogramColours( 21 ), xlab = "", ylab = "",
            xaxs = "i", xaxt = "n", yaxs = "i", yaxt = "n",
            ann = TRUE, xaxt = "n", yaxt = "n", FUN = function( x ) FUN_heatFun( length( x ) ), ... )
  LIST_filled <- fill_bins( MAT_dihedrals,
                            INT_xbins = xbins,
                            INT_ybins = ybins )
  if( STRING_plotType == "comic" )
  {
    # plot in a contour plot style
    image( LIST_filled[[ "xBins" ]],
           LIST_filled[[ "yBins" ]],
           FUN_heatFun( LIST_filled[[ "freq2D" ]] ),
           col = PALETTE_histogramColours( 100 ),
           xaxs = "i", xaxt = "n", xlab = "",
           yaxs = "i", yaxt = "n", ylab = "" )
    axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 2.75, cex = 1.25 )
    mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.45, cex = 1.25 )
  }
  if( STRING_plotType == "fancy" )
  {
    DF_frequencies <- as.data.frame( table( findInterval( MAT_dihedrals[ , 1 ],
                                                          LIST_filled[[ "xBins" ]] ),
                                            findInterval( MAT_dihedrals[ , 2 ],
                                                          LIST_filled[[ "yBins" ]] ) ) )
    DF_frequencies[ , 1 ] <- as.numeric( DF_frequencies[ , 1 ] )
    DF_frequencies[ , 2 ] <- as.numeric( DF_frequencies[ , 2 ] )
    freq2D <- diag( LIST_filled[[ "xBins" ]] ) * 0
    freq2D[ cbind( DF_frequencies[ , 1 ], DF_frequencies[ , 2 ] ) ] <- DF_frequencies[ , 3 ]
    INT_numberColours <- 100
    VEC_palette <- PALETTE_histogramColours( INT_numberColours )
    zFacetValue <- freq2D[ -1, -1 ] + 
                   freq2D[ -1, -ncol( freq2D ) ] +
                   freq2D[ -nrow( freq2D ), -1 ] + 
                   freq2D[ -nrow( freq2D ), -ncol( freq2D ) ]
    zFacetCol <- cut( zFacetValue, INT_numberColours )
    par( oma = c( 1.0, 0.0, 0.0, 0.0 ), mar = c( 0.0, 0.0, 4.5, 0.0 ) )
    perspMatrix <- persp( freq2D,
                          col = VEC_palette[ zFacetCol ],
                          box = FALSE,
                          axes = FALSE,
                          theta = 15,
                          r = 3,
                          d = 0.75 )
    lines( trans3d( seq( 0, 1, by = 0.25 ), 0, 0, perspMatrix ), col = "black" )
    lines( trans3d( 0, seq( 0, 1, by = 0.25 ), 0, perspMatrix ), col = "black" )
    #lines( trans3d( 0, 0, seq( 0, max( DF_frequencies[ , 3 ] ), length.out = 4 ), perspMatrix ),
    #       col = "black" )
    
    # x-axis
    tick.start <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), 0, 0, perspMatrix )
    tick.end <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), -0.05, 0, perspMatrix )
    segments( tick.start$x, tick.start$y, tick.end$x, tick.end$y )
    labels <- c( -135, -90, -45, 0, 45, 90, 135 )
    label.pos <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), -0.09, 0, perspMatrix )
    text( label.pos$x, label.pos$y, labels = labels, adj = c( 0, NA ), cex = 0.75 )
    
    # y-axis
    #label.pos <- trans3d( seq( 0, 1, by = 0.25 ), ( 0 - 0.1 ), 0, perspMatrix )
    #text( label.pos$x, label.pos$y, labels = labels, adj = c( 0, NA ), cex = 1.0 )
  }
  #########
  
  # if specified, print all Ramachandran regions and their labels
  if( length( LIST_areas ) > 0 )
    for( i in 1:length( LIST_areas ) )
    {
      if( length( LIST_areas[[ i ]] ) < 3 )
      {
        next
      }
      for( j in 2:length( LIST_areas[[ i ]] ) )
      {
        if( length( LIST_areas[[ i ]] ) > j )
        {
          segments( LIST_areas[[ i ]][[ j ]][[ 1 ]], LIST_areas[[ i ]][[ j ]][[ 2 ]], 
                    LIST_areas[[ i ]][[ j + 1 ]][[ 1 ]], LIST_areas[[ i ]][[ j + 1 ]][[ 2 ]],
                    col = "black", lwd = 2 )
        }
        else
        {
          segments( LIST_areas[[ i ]][[ j ]][[ 1 ]], LIST_areas[[ i ]][[ j ]][[ 2 ]], 
                    LIST_areas[[ i ]][[ 2 ]][[ 1 ]], LIST_areas[[ i ]][[ 2 ]][[ 2 ]],
                    col = "black", lwd = 2 )
        }
      }
      cen <- calculate_mid( LIST_areas[[ i ]][ -1 ] )
      text( cen[[ 1 ]], cen[[ 2 ]], labels = LIST_areas[[ i ]][[ 1 ]], cex = 2.15 )
    }
  #########
}