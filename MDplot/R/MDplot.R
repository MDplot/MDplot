# load required packages
library( MASS )
library( RColorBrewer )
library( gplots )
library( calibrate )
# definitions
VEC_xTicks <- c( -179.9, -90, 0, 90, 180 )
VEC_xLabels <- c( -180, -90, 0, 90, 180 )
PALETTE_histogramColours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )

split_equidistant <- function( VEC_values, n = 5 )
{
  lower_bound = round( VEC_values[[ 1 ]], digits = - ( log10( VEC_values[[ 2 ]] ) - 1 ) )
  upper_bound = round( VEC_values[[ 2 ]], digits = - ( log10( VEC_values[[ 2 ]] ) - 1 ) )
  VEC_return <- c( lower_bound )
  delta <- upper_bound - lower_bound
  it <- delta / ( n - 1 )
  for( i in 1:( n - 2 ) )
  {
    VEC_return <- c( VEC_return, as.integer( lower_bound + it * i ) )
  }
  VEC_return <- c( VEC_return, upper_bound )
  return( VEC_return )
}

calculate_mid <- function( LIST_points )
{
  REAL_x = 0
  REAL_y = 0
  for( i in 1:length( LIST_points ) )
  {
    REAL_x <- REAL_x + LIST_points[[ i ]][[ 1 ]]
    REAL_y <- REAL_y + LIST_points[[ i ]][[ 2 ]]
  }
  return( c( ( REAL_x / length( LIST_points ) ), ( REAL_y / length( LIST_points ) ) ) )
}

MDplot_ramachandran <- function( MAT_dihedrals, xbins = 450, ybins = 450, heatFun = "log", 
                                 plotTitle = "Ramachandran plot", LIST_areas = c( ), ... )
{
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

MDplot_RMSD <- function( MAT_datainput, BOOL_frax = TRUE, REAL_division_factor = 1, plotTitle = "RMSD plot", xunit = "ns", ... )
{
  if( !BOOL_frax )
  {
    newRow <- seq( 1:nrow( MAT_datainput ) )
    MAT_datainput <- cbind( newRow, MAT_datainput )
  }
  PALETTE_RMSD_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_RMSD <- PALETTE_RMSD_colours( ( ncol( MAT_datainput ) - 1 ) )
  TS_datainput <- as.ts( MAT_MDplot_RMSD_example[ , -1 ] )
  plot.ts( TS_datainput, col = COLOURS_RMSD, plot.type = "single", xaxs = "i", xaxt = "n", xlab = "", ylab = "", ... )
  axis( 1, at = split_equidistant( c( 1, nrow( MAT_datainput ) ), 7 ), labels = split_equidistant( c( 1, ( nrow( MAT_datainput ) / REAL_division_factor ) ), 7 ), cex.axis = 1 )
  mtext( side = 1, text = paste( "time [", xunit, "]" ), line = 3, cex = 1.75 )
  mtext( side = 2, text = "RMSD", line = 2.4, cex = 1.75 )
  title( plotTitle )
  legend( "topright", legend = colnames( MAT_MDplot_RMSD_example[ , -1 ] ), col = COLOURS_RMSD, lty = 1, cex = 1 )
}

MDplot_DSSP_summary <- function( TABLE_datainput, BOOL_printLegend = FALSE, COLOURS_DSSP_summary = NULL, VEC_showValues = NULL )
{
  VEC_residues <- TABLE_datainput[ , 1 ]
  TABLE_datainput <- TABLE_datainput[ , -1 ]
  MAT_data <- as.matrix( TABLE_datainput[ , c( F, T ) ] )
  if( is.null( COLOURS_DSSP_summary ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    COLOURS_DSSP_summary <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  if( is.null( VEC_showValues  ) )
  {
    VEC_showValues = rep( 1:ncol( MAT_data ) )
  }
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
  {
    if( !( i %in% VEC_showValues ) )
    {
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
    }
  }
  MAT_data <- MAT_buffer
  if( BOOL_printLegend )
  {
    par( mar = c( 4.5, 4.5, 2.5, 12 ) )
  }
  else
  { 
    par( mar = c( 4.5, 4.5, 2.5, 2.5 ) )
  }
  plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
        xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), col = COLOURS_DSSP_summary, 
        xlab = "residues", ylab = "fractions [%]",  xaxs = "i", yaxs = "i", 
       cex = 0.75, pch = 19 )
  if( nrow( MAT_data ) > 1 )
  {
    for( i in 2:nrow( MAT_data ) )
    {
      par( new = TRUE )
      plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ), MAT_data[ i, ], col = COLOURS_DSSP_summary, 
            xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
            xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), cex = 0.75, pch = 19 )
    }
  }
  if( BOOL_printLegend )
  {
    par( xpd = TRUE )
    legend( 110, 75, legend = colnames( MAT_data ), col = COLOURS_DSSP_summary, lty = 1, cex = 1 )
    par( xpd = FALSE )
  }
}

MDplot_DSSP_timeseries <- function( VEC_timeseries )
{
  
}