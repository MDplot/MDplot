# load noe data
load_noe <- function( files,
                      mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )

  # get number of violations
  VEC_fileRead <- scan( files[ 1 ], what = "",
                        sep = "\n", quiet = TRUE )
  INT_pos <- which( VEC_fileRead == "AVERAGE NOE VIOLATIONS" ) + 5
  INT_number <- 0
  for( i in INT_pos:length( VEC_fileRead ) )
    if( VEC_fileRead[ i ] != "END" )
      INT_number <- INT_number + 1
    else
      break
  
  LIST_result <- list()
  for( j in 1:length( files ) )
  {
    VEC_fileRead <- scan( files[ j ], what = "",
                          sep = "\n", quiet = TRUE )
    INT_pos <- which( VEC_fileRead == "VIOLATION DISTRIBUTION" ) + 2
    MAT_buffer <- NULL
    for( i in INT_pos:length( VEC_fileRead ) )
    {
      if( VEC_fileRead[ i ] != "END" )
      {
        VEC_split <- unlist( strsplit( VEC_fileRead[ i ], "[[:space:]]+" ) )
        VEC_split <- as.numeric( VEC_split[ VEC_split != "" ] )
        if( is.null( MAT_buffer ) )
          MAT_buffer <- matrix( VEC_split,
                                ncol = 2 )
        else
          MAT_buffer <- rbind( MAT_buffer,
                               VEC_split )
      }
      else
        break
    }
    LIST_result[[ length( LIST_result ) + 1 ]] <- MAT_buffer
  }
  REAL_min <- min( as.numeric( unlist( LIST_result[[ 1 ]][ , 1 ] ) ) )
  REAL_max <- max( as.numeric( unlist( LIST_result[[ 1 ]][ , 1 ] ) ) )
  for( i in 1:length( LIST_result ) )
  {
    REAL_min <- min( c( REAL_min,
                        min( as.numeric( unlist( LIST_result[[ i ]][ , 1 ] ) ) ) ) )
    REAL_max <- max( c( REAL_max,
                        min( as.numeric( unlist( LIST_result[[ i ]][ , 1 ] ) ) ) ) )
  }
  MAT_violationDistribution <- matrix( round( seq.int( from = REAL_min,
                                                       to = REAL_max,
                                                       by = abs( as.numeric( unlist( LIST_result[[ 1 ]][ 1, 1 ] ) ) -
                                                                as.numeric( unlist( LIST_result[[ 1 ]][ 2, 1 ] ) ) ) ),
                                              digits = 3 ),
                                       ncol = 1 )
  for( i in 1:length( LIST_result ) )
  {
    MAT_violationDistribution <- cbind( MAT_violationDistribution,
                                        rep( 0,
                                             times = nrow( MAT_violationDistribution ) ) )
    MAT_cur <- LIST_result[[ i ]]
    for( j in 1:nrow( MAT_cur ) )
    {
      MAT_violationDistribution[ MAT_violationDistribution[ , 1 ] == MAT_cur[ j, 1 ], 1 + i ] <- MAT_cur[ j, 2 ]
    }
  }
  return( list( MAT_violationDistribution, INT_number ) )
}

# plot noe data
noe <- function( noeData,
                 printPercentages = TRUE,
                 colours = NA,
                 lineTypes = NA,
                 names = NA,
                 plotSumCurves = TRUE,
                 maxYAxis = NA,
                 printLegend = FALSE,
                 ... )
{
  # remove "negative NOEs"
  totalNOEs <- noeData[[ 2 ]]
  noeData <- noeData[[ 1 ]]
  noeData <- noeData[ noeData[ , 1 ] > 0, ]
  
  # set colours vector
  if( all( is.na( colours ) ) )
    colours <- c( "grey", "white", "lightgrey", "black", "blue" )
  
  # set names vector
  if( all( is.na( names ) ) )
    names <- seq( from = 1, to = ncol( noeData ) - 1, by = 1 )
  
  # set line types
  if( all( is.na( lineTypes ) ) )
    lineTypes <- c( "solid", "dotted", "longdash", "dashed", "twodash" )

  # calculate percentages in case it is specified
  if( printPercentages )
    for( i in 2:ncol( noeData ) )
      noeData[ , i ] <- round( noeData[ , i ] / totalNOEs * 100,
                               digits = 3 )
  
  # set yaxis-limits
  REAL_maxYValueSum <- 0
  REAL_maxYValue <- 0
  for( i in 2:ncol( noeData ) )
  {
    REAL_maxYValueSum <- max( REAL_maxYValueSum,
                              sum( noeData[ , i ] ) )
    REAL_maxYValue <- max( REAL_maxYValue,
                           noeData[ , i ] )
  }
  if( plotSumCurves )
    VEC_yLimits <- c( 0, REAL_maxYValueSum * 1.05 )
  else
    VEC_yLimits <- c( 0, REAL_maxYValue * 1.05 )
  if( !is.na( maxYAxis ) )
      VEC_yLimits[ 2 ] <- maxYAxis

  # plot histogram now
  par( mar = c( 5.6, 4.25, 4.25, 2.0 ) )
  VEC_spaceArgument <- 0
  MAT_toPlot <- NULL
  if( ncol( noeData ) > 2 )
  {
    VEC_spaceArgument <- c( 0, 0 )
    MAT_toPlot <- t( noeData[ , 2:ncol( noeData ) ] )
  }
  else
    MAT_toPlot <- noeData[ , 2 ]
  VEC_positions <- barplot( MAT_toPlot,
                            ylim = VEC_yLimits, xlim = c( 0, ( ncol( noeData ) - 1 ) * nrow( noeData ) ),
                            xaxs = "i", yaxs = "i",
                            xlab = "", xaxt = "n",
                            ylab = "", col = colours[ 1:( ncol( noeData ) - 1 ) ],
                            beside = TRUE, space = VEC_spaceArgument,
                            ... )
  axis( 1,
        at = seq( from = 0,
                  to = ( ( ncol( noeData ) - 1 ) * nrow( noeData ) ) - 1,
                  by = ( ncol( noeData ) - 1 ) ),
        labels = c( noeData[ , 1 ] ),
        las = 3,
        cex = 0.85 )
  mtext( text = "noe violations", side = 1, line = 4.15, cex = 1.0 )
  STRING_yAxis <- "fraction violations"
  if( printPercentages )
    STRING_yAxis <- paste( STRING_yAxis, " [%]", sep = "" )
  mtext( text = STRING_yAxis, side = 2, line = 2.75, cex = 1.0 )

  # add addition curves
  for( j in 2:ncol( noeData ) )
  {
    if( !plotSumCurves )
      next

    # plot in same device
    par( new = TRUE )

    # calculate NOE curve
    MAT_theCurve <- matrix( c( noeData[ , 1 ],
                               rep( NA, times = nrow( noeData ) ) ),
                            nrow = nrow( noeData ), byrow = FALSE )
    for( i in 1:nrow( MAT_theCurve ) )
      MAT_theCurve[ i, 2 ] <- sum( noeData[ 1:i, j ] )
    if( ncol( noeData ) > 2 )
      MAT_theCurve[ , 1 ] <- VEC_positions[ j - 1, ]
    else
      MAT_theCurve[ , 1 ] <- VEC_positions

    # plot the curve
    plot( MAT_theCurve,
          bty = "n", type = "l", xaxs = "i",
          xaxt = "n", yaxs = "i", yaxt = "n",
          xlab = "", ylab = "",
          lty = lineTypes[ j - 1 ], xlim = c( 0, ( ncol( noeData ) - 1 ) * nrow( noeData ) ),
          ylim = VEC_yLimits, lwd = 1.75 )
  }
  
  # plot legend now
  if( printLegend )
  {
    legend( "right",
            legend = names,
            fill = colours[ 1:( ncol( noeData ) - 1 ) ], 
            bty = "n",
            cex = 1.0,
            horiz = FALSE )
  }
  
  return( noeData )
}