# plot RMSD
MDplot_RMSD <- function( MAT_datainput, BOOL_frax = TRUE, REAL_division_factor = 1, plotTitle = "RMSD plot", xunit = "ns", ... )
{
  if( !BOOL_frax )
  {
    newRow <- seq( 1:nrow( MAT_datainput ) )
    MAT_datainput <- cbind( newRow, MAT_datainput )
  }
  
  # plot the RMSD curves
  PALETTE_RMSD_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_RMSD <- PALETTE_RMSD_colours( ( ncol( MAT_datainput ) - 1 ) )
  TS_datainput <- as.ts( MAT_MDplot_RMSD_example[ , -1 ] )
  plot.ts( TS_datainput, col = COLOURS_RMSD, plot.type = "single", xaxs = "i", xaxt = "n", xlab = "", ylab = "", ... )
  axis( 1, at = split_equidistant( c( 1, nrow( MAT_datainput ) ), 7 ), labels = split_equidistant( c( 1, ( nrow( MAT_datainput ) / REAL_division_factor ) ), 7 ), cex.axis = 1 )
  mtext( side = 1, text = paste( "time [", xunit, "]" ), line = 3, cex = 1.75 )
  mtext( side = 2, text = "RMSD", line = 2.4, cex = 1.75 )
  title( plotTitle )
  legend( "topright", legend = colnames( MAT_MDplot_RMSD_example[ , -1 ] ), col = COLOURS_RMSD, lty = 1, cex = 1 )
  #########
}