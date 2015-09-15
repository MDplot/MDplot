MDplot_ramachandran <- function( MAT_dihedrals, xbins = 450, ybins = 450, heatFun = "log", 
                                 plotTitle = "Ramachandran plot", LIST_areas = c( ), ... )
{
  VEC_xTicks <- c( -179.9, -90, 0, 90, 180 )
  VEC_xLabels <- c( -180, -90, 0, 90, 180 )
  PALETTE_histogramColours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  FUN_heatFun = function( x ) log( length( x ) )
  if( heatFun == "log" )
  {}
  else if( heatFun == "norm" )
  {
    FUN_heatFun = function( x ) x
  }
  else
  {
    stop( paste( "Error: the function '", heatFun, "' for parameter 'heatFun' is not defined.", sep = "" ) )
  }
  hist2d( MAT_dihedrals, nbins = c( xbins, ybins ), same.scale = FALSE, na.rm = TRUE, 
          show = TRUE, col = PALETTE_histogramColours( 15 ), xlab = "", ylab = "", xaxs = "i", 
          yaxs = "i", ann = TRUE, xaxt = "n", yaxt = "n", FUN = FUN_heatFun, ... )
  if( length( LIST_areas ) > 0 )
  {
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
      text( cen[[ 1 ]], cen[[ 2 ]], labels = LIST_areas[[ i ]][[ 1 ]], cex = 1.5 )
    }
  }
  axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1 )
  axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1)
  mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 3, cex = 1.75 )
  mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.4, cex = 1.75 )
  title( plotTitle )
}