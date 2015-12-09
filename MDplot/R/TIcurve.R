# load the lambda point information here
load_TIcurve <- function( files )
{
  LIST_files <- list()
  for( i in 1:length( files ) )
    LIST_files[[ length( LIST_files ) + 1 ]] <- as.matrix( read.table( files[ i ] ) )
  return( LIST_files )
}

# plot the curve and calculate and plot the integral
TIcurve <- function( lambdas,
                     invertedBackwards = FALSE,
                     ... )
{
  
  # calculate maximum / minimum and colours / do inversion if necessary
  VEC_errorLimits <- c( min( unlist( lapply( lambdas,
                                             FUN = function( x ) min( x[ , 3 ] ) ) ) ),
                        max( unlist( lapply( lambdas,
                                             FUN = function( x ) max( x[ , 3 ] ) ) ) ) )
  VEC_valueLimits <- c( min( unlist( lapply( lambdas,
                                             FUN = function( x ) min( x[ , 2 ] ) ) ) ) -
                        VEC_errorLimits[ 2 ] * 1.25,
                        max( unlist( lapply( lambdas,
                                             FUN = function( x ) max( x[ , 2 ] ) ) ) ) +
                        VEC_errorLimits[ 2 ] * 1.25 )
  VEC_colours <- c( "black", "red" )
  if( length( lambdas ) > 1 &&
      invertedBackwards )
    lambdas[[ 2 ]][ , 2 ] <- rev( sapply( lambdas[[ 2 ]][ , 2 ],
                                             FUN = function( x ) x * ( -1 ) ) )
  #########
  
  # set proper outer margins and plot it
  if( length( lambdas ) > 1 )
    par( oma = c( 3.25, 3.00, 0.45, 0.0 ) )
  else
    par( oma = c( 1.35, 3.00, 0.45, 0.0 ) )
  for( i in 1:length( lambdas ) )
  {
    if( i == 1 )
    {
      TIplot <- plot( lambdas[[ i ]],
                      ylim = VEC_valueLimits,
                      ylab = "",
                      xaxs = "i", xlab = "",
                      pch = 19, cex = 0.6,
                      col = VEC_colours[ i ],
                      ... )
      mtext( side = 1, text = expression( lambda ), line = 3, cex = 1.45 )
      mtext( side = 2, 
             text = expression( atop( "<"*frac( paste( partialdiff, "H" ),
                                                paste( partialdiff, lambda ) )*">",
                                      atop( "[kJ/mol]",
                                            "" ) ) ),
             line = 2.4, cex = 1.75, las = 1 )
      abline( h = 0, lwd = 1, lty = 3 )
    }
    else
      TIplot <- plot( lambdas[[ i ]],
                      ylim = VEC_valueLimits,
                      ylab = "", yaxt = "n",
                      xaxs = "i", xaxt = "n", xlab = "",
                      pch = 19, cex = 0.6,
                      col = VEC_colours[ i ],
                      ... )
  plot_segments( lambdas[[ i ]][ , 1:2 ],
                 lambdas[[ i ]][ , 3 ],
                 0.01,
                 col = VEC_colours[ i ] )
  par( new = TRUE )
  plot( lambdas[[ i ]],
        ylim = VEC_valueLimits, yaxt = "n", ylab = "",
        xaxs = "i", xaxt = "n", xlab = "",
        type = "l", lwd = 1, col = VEC_colours[ i ] )
  par( new = TRUE )
  }
  #########
  
  # integrate over curves
  REAL_forward_integral <- unlist( integrate_curve( lambdas[[ 1 ]] )[ "integral" ] )
  REAL_forward_error <- unlist( integrate_curve( lambdas[[ 1 ]] )[ "error" ] )
  INT_significantForward <- get_sign_digits( REAL_forward_error )
  REAL_forward_integral_rounded <- round( REAL_forward_integral, digits = INT_significantForward )
  REAL_forward_error_rounded <- round( REAL_forward_error, digits = INT_significantForward )
  mtext( side = 1, line = 4.75, cex = 1.0,
         adj = 1,
         text = substitute( paste( Delta, "G"["forw"], " = ",
                                   REAL_forward_integral_rounded,
                                   #" \u00b1 ",
                                   " +/- ",
                                   REAL_forward_error_rounded,
                                   " [kJ/mol]" ) ) )
  if( length( lambdas ) > 1 )
  {
    REAL_backward_integral <- unlist( integrate_curve( lambdas[[ 2 ]] )[ "integral" ] )
    REAL_backward_error <- unlist( integrate_curve( lambdas[[ 2 ]] )[ "error" ] )
    INT_significantBackward <- get_sign_digits( REAL_backward_error )
    REAL_backward_integral_rounded <- round( REAL_backward_integral, digits = INT_significantBackward )
    REAL_backward_error_rounded <- round( REAL_backward_error, digits = INT_significantBackward )
    mtext( side = 1, line = 6.0, cex = 1.0,
           adj = 1,
           text = substitute( paste( Delta, "G"["back"], " = ",
                                     REAL_backward_integral_rounded,
                                     #" \u00b1 ",
                                     " +/- ",
                                     REAL_backward_error_rounded,
                                     " [kJ/mol]" ) ) )
    REAL_hysteresis <- round( REAL_forward_integral - REAL_backward_integral,
                              digits = min( c( INT_significantForward,
                                               INT_significantBackward ) ) )
    mtext( side = 1, line = 7.25, cex = 1.0,
           adj = 1,
           text = substitute( paste( "hysteresis = ",
                                     REAL_hysteresis,
                                     " [kJ/mol]" ) ) )
  }
  #########
}