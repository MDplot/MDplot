# load the lambda point information here
MDplot_load_TIcurve <- function( STRING_path )
{
  return( as.matrix( read.table( STRING_path ) ) )
}

# plot the curve and calculate and plot the integral
MDplot_TIcurve <- function( MAT_input,
                            ... )
{
  # set proper outer margins and plot it
  par( oma = c( 1.35, 3.00, 0.45, 0.0 ) )
  TIplot <- plot( MAT_input, ylim = c( min( MAT_input[ , 2 ] ), max( MAT_input[ , 2 ] ) ),
                  xaxs = "i", pch = 19, cex = 0.6, xlab = "", ylab = "",
                  ... )
  plot_segments( MAT_input[ , 1:2 ], MAT_input[ , 3 ], 0.01 )
  mtext( side = 1, text = expression( lambda ), line = 3, cex = 1.45 )
  mtext( side = 2, 
         text = expression( atop( "<"*frac( paste( partialdiff, "H" ), paste( partialdiff, lambda ) )*">",
                                   atop( "[kJ/mol]",
                                         "" ) ) ),
         line = 2.4, cex = 1.75, las = 1 )
  par( new = TRUE )
  plot( MAT_input, ylim = c( min( MAT_input[ , 2 ] ), max( MAT_input[ , 2 ] ) ), xaxs = "i",
        type = "l", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "" )
  #########
  
  # integrate over curve
  REAL_integral <- round( unlist( integrate_curve( MAT_input )[ "integral" ] ), 2 )
  REAL_error <- round( unlist( integrate_curve( MAT_input )[ "error" ] ), 2 )
  mtext( side = 1, line = 5.0, cex = 1.25,
         adj = 1,
         text = substitute( paste( Delta,
                                   "G = ",
                                   REAL_integral,
                                   " +/- ",
                                   #" \u00b1 ",
                                   REAL_error,
                                   " [kJ/mol]" ) ) )
  #########
  
  # draw dotted line at zero average derivative with respect to lambda
  abline( h = 0, lwd = 1, lty = 3 )
  #########
}