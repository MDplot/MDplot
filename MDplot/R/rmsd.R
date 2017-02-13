# plot the average RMSD of many runs with spread
rmsd_average <- function( rmsdInput,
                          skip = 0,
                          printMeans = FALSE,
                          levelFactor = NA,
                          snapshotsPerTimeInt = 1000,
                          timeUnit = "ns",
                          rmsdUnit = "nm",
                          maxYAxis = NA,
                          barePlot = FALSE,
                          ... )
{
  # initialize
  MAT_values <- matrix( c( 1:length( rmsdInput[[ 1 ]][[ 1 ]] ),
                           rep( NA, times = 3 * length( rmsdInput[[ 1 ]][[ 1 ]] ) ) ),
                        byrow = FALSE, ncol = 4 )
  MAT_input <- matrix( rmsdInput[[ 1 ]][[ 2 ]], ncol = 1 )
  for( i in 2:length( rmsdInput ) )
    MAT_input <- cbind( MAT_input,
                        rmsdInput[[ i ]][[ 2 ]] )
  colnames( MAT_values ) <- c( "snapshot", "minimum", "mean", "maximum" )
  REAL_max_RMSD = max( MAT_input )
  if( !is.na( maxYAxis ) )
    REAL_max_RMSD = maxYAxis
  
  # calculate the average RMSD, minimum and maximum values
  for( i in 1:length( rmsdInput[[ 1 ]][[ 1 ]] ) )
  {
    MAT_values[ i, 2 ] <- min( MAT_input[ i, ] )
    MAT_values[ i, 3 ] <- mean( MAT_input[ i, ] )
    MAT_values[ i, 4 ] <- max( MAT_input[ i, ] )
  }
  if( !is.na( levelFactor ) )
    MAT_values <- MAT_values[ c( T, rep( F, times = levelFactor - 1 ) ), ]
  #########
  
  # plot the minimum curve
  plot( MAT_values[ , 1 ], MAT_values[ , 2 ], type = "l",
        col = "grey", xaxs = "i", yaxs = "i",
        xaxt = "n",
        xlim = c( 0, nrow( MAT_values ) ),
        #yaxt = ifelse( barePlot, "n", "s" ),
        xlab = "", ylab = "",
        ylim = c( 0, REAL_max_RMSD * 1.05 ), ... ) #, xlim = c( 0, INT_max_snapshot ), ... )
  if( !barePlot )
  {
    VEC_tickValues <- axTicks( 1 )
    axis( 1,
          at = VEC_tickValues,
          labels = VEC_tickValues / snapshotsPerTimeInt )
    mtext( side = 1, text = paste( "time [", timeUnit, "]", sep = "" ), line = 3,
           cex = 1 )
    mtext( side = 2, text = paste( "RMSD [", rmsdUnit, "]", sep = "" ), line = 2.75,
           cex = 1 )
  }
  par( new = TRUE )
  
  # plot the maximum curve
  plot( MAT_values[ , 1 ], MAT_values[ , 4 ], type = "l",
        col = "grey", xaxs = "i", yaxs = "i",
        xaxt = "n", yaxt = "n", xlim = c( 0, nrow( MAT_values ) ),
        xlab = "", ylab = "",
        ylim = c( 0, REAL_max_RMSD * 1.05 ) ) #, xlim = c( 0, INT_max_snapshot ), ... )
   par( new = TRUE )
  
  # plot the mean curve
  plot( MAT_values[ , 1 ], MAT_values[ , 3 ], type = "l",
        col = "black", xaxs = "i", yaxs = "i",
        xaxt = "n", yaxt = "n", xlim = c( 0, nrow( MAT_values ) ),
        xlab = "", ylab = "", cex = 1.45,
        ylim = c( 0, REAL_max_RMSD * 1.05 ) ) #, xlim = c( 0, INT_max_snapshot ), ... 
  return( MAT_values )
}

# load RMSD
load_rmsd <- function( files,
                       mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" &&
      mdEngine != "AMBER" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )
  LIST_return <- list()
  for( i in 1:length( files ) )
  {
    TABLE_input <- NA
    if( mdEngine == "GROMOS" )
    {
      TABLE_input <- read.table( files[ i ] )
    }
    if( mdEngine == "GROMACS" )
    {
      inputData <- readLines( files[ i ],
                              warn = FALSE )
      inputData <- gsub( "^[#].*", "", inputData )
      inputData <- gsub( "^[@].*", "", inputData )
      inputData <- gsub( "^\\s+|\\s+$", "", inputData )
      inputData <- inputData[ inputData != "" ]
      VEC_input <- as.numeric( unlist( strsplit( inputData, "\\s+" ) ) )
      TABLE_input <- matrix( VEC_input, byrow = TRUE, ncol = 2 )
    }
    if( mdEngine == "AMBER" )
    {
      inputData <- readLines( files[ i ],
                              warn = FALSE )
      inputData <- gsub( "^[#].*", "", inputData )
      inputData <- gsub( "^[@].*", "", inputData )
      inputData <- gsub( "^\\s+|\\s+$", "", inputData )
      inputData <- inputData[ inputData != "" ]
      VEC_input <- as.numeric( unlist( strsplit( inputData, "\\s+" ) ) )
      TABLE_input <- matrix( VEC_input, byrow = TRUE, ncol = 2 )
    }
    if( length( LIST_return ) == 0 )
      LIST_return <- list( TABLE_input[ , 1 ], TABLE_input[ , 2 ] )
    else
    {
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 1 ]
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 2 ]
    }
  }
  return( LIST_return )
}

# plot RMSD
rmsd <- function( rmsdData,
                  printLegend = TRUE,
                  snapshotsPerTimeInt = 1000,
                  timeUnit = "ns",
                  rmsdUnit = "nm",
                  colours = NA,
                  names = NA,
                  legendPosition = "bottomright",
                  barePlot = FALSE,
                  ... )
{
  # get boundaries
  REAL_max_RMSD = max( unlist( lapply( rmsdData[ c( F, T ) ],
                                       FUN = function( x ) max( x ) ) ) )
  INT_max_snapshot = max( unlist( lapply( rmsdData[ c( T, F ) ],
                                           FUN = function( x ) max( x ) ) ) )
  INT_min_snapshot = min( unlist( lapply( rmsdData[ c( T, F ) ],
                                          FUN = function( x ) min( x ) ) ) )
  #########
  print( INT_max_snapshot )
  print( INT_min_snapshot )
  
  # set colours and names
  PALETTE_RMSD_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.na( colours ) )
    colours <- PALETTE_RMSD_colours( length( rmsdData ) / 2 )
  if( all( is.na( names ) ) )
    names = 1:( length( rmsdData ) / 2 )
  #########
  
  LIST_return <- list()
  # plot
  for( i in 1:length( rmsdData ) )
  {
    if( i %% 2 == 1 )
    {
      if( i == 1 )
        plot( rmsdData[[ i ]], rmsdData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n",
              yaxt = ifelse( barePlot, "n", "s" ),
              xlab = "", ylab = "",
              ylim = c( 0, REAL_max_RMSD * 1.05 ), xlim = c( 0, INT_max_snapshot ), ... )
      else
        plot( rmsdData[[ i ]], rmsdData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", yaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_max_RMSD * 1.05 ), xlim = c( 0, INT_max_snapshot ) )
      LIST_return[[ length( LIST_return ) + 1 ]] <- list( minValue = min( rmsdData[[ ( i + 1 ) ]] ),
                                                          maxValue = max( rmsdData[[ ( i + 1 ) ]] ),
                                                          meanValue = mean( rmsdData[[ ( i + 1 ) ]] ),
                                                          sd = sd( rmsdData[[ ( i + 1 ) ]] ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot the rest
  if( !barePlot )
  {
    VEC_timeTicks <- axTicks( 1,
                              usr = c( INT_min_snapshot,
                                       INT_max_snapshot ) )
    VEC_timeLabels <- VEC_timeTicks
    if( !is.na( timeUnit ) )
      VEC_timeLabels <- VEC_timeTicks / snapshotsPerTimeInt
    axis( 1,
          at = VEC_timeTicks,
          labels = VEC_timeLabels )
    mtext( side = 1, text = paste( "time [", timeUnit, "]", sep = "" ), line = 3,
           cex = 1 )
    mtext( side = 2, text = paste( "RMSD [", rmsdUnit, "]", sep = "" ), line = 2.75,
           cex = 1 )
  }
  if( printLegend && !barePlot )
    legend( legendPosition,
            title = "Legend",
            legend = names,
            col = colours,
            lty = 1.0, lwd = 2.0,
            cex = 1 )
  #########
  
  return( LIST_return )
}