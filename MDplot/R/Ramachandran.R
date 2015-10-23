# load ramachandran data into a matrix, select columns
MDplot_load_ramachandran <- function( STRING_file,
                                      VEC_angleColumns = c( 1, 2 ),
                                      INT_heatColumn = NA,
                                      REAL_shiftAngles = NA )
{
  
  # load and parse matrix, return result
  MAT_buffer <- as.matrix( read.table( STRING_file ) )
  if( ncol( MAT_buffer ) < ifelse( is.na( INT_heatColumn ),
                                   max( VEC_angleColumns ),
                                   max( VEC_angleColumns, INT_heatColumn ) ) )
    stop( paste( "Error while loading and parsing file '",
                 STRING_file, "' since the number of columns is less than ",
                 "the maximum column number specified.",
                 sep = "" ) )
  MAT_input <- MAT_buffer[ , VEC_angleColumns ]
  if( !is.na( REAL_shiftAngles ) )
  {
    MAT_input[ , 1 ] <- MAT_input[ , 1 ] + REAL_shiftAngles
    MAT_input[ , 2 ] <- MAT_input[ , 2 ] + REAL_shiftAngles
  }
  if( !is.na( INT_heatColumn ) )
    MAT_input <- cbind( MAT_input, MAT_buffer[ , INT_heatColumn ] )
  return( MAT_input )
}


# plot the angles on a x = ( -180, 180 ) to y = ( -180, 180 ) area
MDplot_ramachandran <- function( MAT_dihedrals,
                                 xbins = 150,
                                 ybins = 150,
                                 heatFun = "log", 
                                 LIST_areas = c(),
                                 STRING_plotType = "comic",
                                 BOOL_printLegend = FALSE,
                                 STRING_heatUnits = NA,
                                 BOOL_plotContour = FALSE,
                                 ... )
{
  
  # settings (small offset for label printing required)
  VEC_xTicks  <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  VEC_xLabels <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  PALETTE_sparse <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  PALETTE_comic <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  PALETTE_fancy <- colorRampPalette( c( "lightgrey", rev( brewer.pal( 11, 'Spectral' ) ) ) )
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
  
  # plotting
  LIST_filled <- fill_bins( MAT_dihedrals,
                            INT_xbins = xbins,
                            INT_ybins = ybins,
                            VEC_xLim = c( -180, 180 ),
                            VEC_yLim = c( -180, 180 ) )
  if( BOOL_printLegend )
  {
    layout( matrix( 1:2, ncol = 2 ), width = c( 0.75, 0.25 ), height = c( 1, 1 ) )
    par( mar = c( 4.0, 4.0, 4.0, 0.0 ) )
  }
  if( STRING_plotType == "sparse" )
  {
    hist2d( MAT_dihedrals, nbins = c( xbins, ybins ), same.scale = FALSE, na.rm = TRUE, 
            show = TRUE, col = PALETTE_sparse( 21 ), xlab = "", ylab = "",
            xaxs = "i", xaxt = "n", yaxs = "i", yaxt = "n",
            ann = TRUE, xaxt = "n", yaxt = "n", FUN = function( x ) FUN_heatFun( length( x ) ),
            xlim = c( -180, 180 ), ylim = c( -180, 180 ), ... )
    axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 2.75, cex = 1.45 )
    mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.45, cex = 1.45 )
  }
  if( STRING_plotType == "comic" )
  {
    # heatmap
    image( LIST_filled[[ "xBins" ]],
           LIST_filled[[ "yBins" ]],
           FUN_heatFun( LIST_filled[[ "freq2D" ]] ),
           col = PALETTE_comic( 100 ),
           xaxs = "i", xaxt = "n", xlab = "",
           yaxs = "i", yaxt = "n", ylab = "",
           ... )
    ##########
    
    # contour
    # TODO: separate this functionality somehow
    if( BOOL_plotContour && ncol( MAT_dihedrals ) < 3 )
    {
      DF_frequencies <- as.data.frame( table( findInterval( MAT_dihedrals[ , 1 ],
                                                            LIST_filled[[ "xBins" ]] ),
                                              findInterval( MAT_dihedrals[ , 2 ],
                                                            LIST_filled[[ "yBins" ]] ) ) )
      DF_frequencies[ , 1 ] <- as.numeric( DF_frequencies[ , 1 ] )
      DF_frequencies[ , 2 ] <- as.numeric( DF_frequencies[ , 2 ] )
      freq2D <- diag( LIST_filled[[ "xBins" ]] ) * 0
      freq2D[ cbind( DF_frequencies[ , 1 ], DF_frequencies[ , 2 ] ) ] <- DF_frequencies[ , 3 ]
      contour( LIST_filled[[ "xBins" ]],
               LIST_filled[[ "yBins" ]],
               freq2D,
               add = TRUE,
               drawlabels = FALSE )
    }
    #########
    
    axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
    mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 2.75, cex = 1.45 )
    mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.45, cex = 1.45 )
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
    VEC_palette <- PALETTE_fancy( INT_numberColours )
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
    #lines( trans3d( 0, seq( 0, 1, by = 0.25 ), 0, perspMatrix ), col = "black" )
    #lines( trans3d( 0, 0, seq( 0, max( DF_frequencies[ , 3 ] ), length.out = 4 ), perspMatrix ),
    #       col = "black" )
    
    # x-axis
    tick.start <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), 0, 0, perspMatrix )
    tick.end <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), -0.05, 0, perspMatrix )
    segments( tick.start$x, tick.start$y, tick.end$x, tick.end$y )
    labels <- c( -135, -90, -45, 0, 45, 90, 135 )
    label.pos <- trans3d( seq( 1 / 8, 7 / 8, by = 1 / 8 ), -0.09, 0, perspMatrix )
    text( label.pos$x, label.pos$y, labels = labels, adj = c( 0, NA ), cex = 0.9 )
    
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
  
  # print legend if specified to do so
  if( BOOL_printLegend && ncol( MAT_dihedrals ) == 3 )
  {
    legend_image <- as.raster( matrix( PALETTE_comic( 100 ), ncol = 1 ) )
    par( mar = c( 4.0, 1.0, 4.0, 2.5 ) )
    STRING_legendCaption <- ifelse( is.na( STRING_heatUnits ),
                                    "Legend",
                                    paste( "Legend ", STRING_heatUnits, sep = "" ) )
    plot( c( 0, 2 ), c( 0, 1 ), type = 'n',
          axes = F, xlab = '', ylab = '',
          main = STRING_legendCaption )
    text( x = 1.5, y = seq( 0, 1, l = 5 ),
          labels = round( seq( min( MAT_dihedrals[ , 3 ] ),
                               max( MAT_dihedrals[ , 3 ] ),
                               l = 5 ), digits = 2 ) )
    rasterImage( legend_image, 0, 0, 1, 1 )
  }
  #########
}