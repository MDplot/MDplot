# load the tmc (total membrane curvature) input
load_mem_tmc <- function( path,
                          format = "MANTICORE" )
{
  format <- toupper( format )
  if( format != "MANTICORE" )
    stop( paste( "The specified 'format', set to ", format, " is unknown.", sep = "" ) )
  
  if( format == "MANTICORE" )
  {
    # fill
  }
}

# tmc (total membrane curvature) plot
mem_tmc <- function( tmcValues,
                     meanOfSurfaces = FALSE,
                     printLegend = TRUE,
                     barePlot = FALSE,
                     ... )
{
  # check input
  if( !is.list( tmcValues ) || length( tmcValues ) < 1 )
    stop( "Input parameter \"tmcValues\" must be a list with at least one member." )

  # generate time-series for all trajectories
  LIST_curves <- list()
  for( indexTraj in 1:length( tmcValues ) )
  {
    # initialize variables
    curTraj <- tmcValues[[ indexTraj ]]
    matCurTraj <- NULL
    if( meanOfSurfaces ) {
    matCurTraj <- matrix( c( 1:length( curTraj ),
                             rep( NA, times = 2 * length( curTraj ) ) ),
                             ncol = 3,
                             byrow = FALSE )
    } else {
      matCurTraj <- matrix( c( 1:length( curTraj ),
                               rep( NA, times = length( curTraj[[ 1 ]] ) * length( curTraj ) ) ),
                            ncol = length( curTraj[[ 1 ]] ) + 1,
                            byrow = FALSE ) }

    # for every snapshot of the current trajectory, get the value and add it to the matrix
    for( indexSnapshot in 1:length( curTraj ) )
    {
      curSnapshot <- curTraj[[ indexSnapshot ]]
      if( meanOfSurfaces ) {
        matCurTraj[ indexSnapshot, 2:3 ] <- c( mean( sapply( X = curSnapshot, FUN = "[[", "globalCurvature" ) ),
                                               sd( sapply( X = curSnapshot, FUN = "[[", "globalCurvature" ) ) )
      } else {
        matCurTraj[ indexSnapshot, 2:ncol( matCurTraj ) ] <- as.vector( sapply( X = curSnapshot, FUN = "[[", "globalCurvature" ) )
      }
    }

    # add curve to list of curves
    LIST_curves[[ length( LIST_curves ) + 1 ]] <- matCurTraj
  }

  # plot the curves
  coloursTrajectories <- c( "red", "blue", "green" )
  linetypesSurfaces <- 1:4
print( LIST_curves )
  # plot the legend (if this is required)
  xlim <- xlim <- c( min( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 1, drop = FALSE ], MARGIN = 2, FUN = min ) ) ) ),
                     max( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 1, drop = FALSE ], MARGIN = 2, FUN = max ) ) ) ) )
  ylim <- NULL
  if( meanOfSurfaces ) {
    
    ylim <- c( min( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 2, drop = FALSE ], MARGIN = 2, FUN = min ) ) ) ),
               max( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 2, drop = FALSE ], MARGIN = 2, FUN = max ) ) ) ) )
  } else {
    ylim <- c( min( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 2:nrow( x ), drop = FALSE ], MARGIN = 2, FUN = min ) ) ) ),
               max( unlist( lapply( X = LIST_curves, FUN = function( x ) apply( X = x[ , 2:nrow( x ), drop = FALSE ], MARGIN = 2, FUN = max ) ) ) ) )
  }
  for( indexCurve in 1:length( LIST_curves ) )
  {
    if( meanOfSurfaces ) {
      if( indexCurve != 1 ) { par( new = TRUE ) }
      plot( x = LIST_curves[[ indexCurve ]][ , 1:2 ], type = 'l', lty = 1,
            col = coloursTrajectories[ indexCurve ], xlim = xlim, ylim = ylim ) }
    else {
      for( indexSurface in 2:ncol( LIST_curves[[ indexCurve ]] ) )
      {
        if( indexCurve != 1 ) { par( new = TRUE ) }
        plot( x = LIST_curves[[ indexCurve ]][ , c( 1, indexSurface ) ], type = 'l', lty = linetypesSurfaces[ indexSurface - 1 ],
              col = coloursTrajectories[ indexCurve ], xlim = xlim, ylim = ylim )
      }
    }
  }

  # return the curves
  return( LIST_curves )
}