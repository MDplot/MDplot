# load ramachandran data into a matrix, select columns
MDplot_load_ramachandran <- function( STRING_file, VEC_columns = c( 3, 4 ) )
{
  
  # load and parse matrix, return result
  MAT_input <- as.matrix( read.table( STRING_file ) )
  if( ncol( MAT_input ) < max( VEC_columns ) )
  {
    stop( paste( "Error while loading and parsing file '", STRING_file, "' since the number ",
                 "of columns is less than the maximum column number specified." ) )
  }
  return( MAT_input[ , VEC_columns ] )
}


# plot the angles on a x = ( -180, 180 ) to y = ( -180, 180 ) area
MDplot_ramachandran <- function( MAT_dihedrals, xbins = 450,
                                 ybins = 450, heatFun = "log", 
                                 LIST_areas = c(), ... )
{
  
  # settings (small offset for label printing required)
  VEC_xTicks  <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  VEC_xLabels <- c( -135,  -90,  -45,    0,   45,   90,  135 )
  PALETTE_histogramColours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  #########
  
  # determine function for plotting (logarithm is useful for lots of points)
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
  #########
  
  # plot the information and use 15 histogram colours
  hist2d( MAT_dihedrals, nbins = c( xbins, ybins ), same.scale = FALSE, na.rm = TRUE, 
          show = TRUE, col = PALETTE_histogramColours( 15 ), xlab = "", ylab = "", xaxs = "i", 
          yaxs = "i", ann = TRUE, xaxt = "n", yaxt = "n", FUN = FUN_heatFun, ... )
  #########
  
  # if specified, print all Ramachandran regions and their labels
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
      text( cen[[ 1 ]], cen[[ 2 ]], labels = LIST_areas[[ i ]][[ 1 ]], cex = 2.15 )
    }
  }
  #########
  
  # print additional plot information
  axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
  axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1.25 )
  mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 3, cex = 1.75 )
  mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.4, cex = 1.75 )
  #########
}