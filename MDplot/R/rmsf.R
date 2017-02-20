# load the RMSF file(s)
load_rmsf <- function( files,
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
      TABLE_input <- read.table( files[ i ] )
    }
    if( length( LIST_return ) == 0 )
    {
      LIST_return <- list( TABLE_input[ , 1 ], TABLE_input[ , 2 ] )
    }
    else
    {
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 1 ]
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 2 ]
    }
  }
  return( LIST_return )
}

# plot RMSF of one or multiple files, combined in one list here
rmsf <- function( rmsfData,
                  printLegend = TRUE,
                  rmsfUnit = "nm",
                  colours = NA,
                  residuewise = FALSE,
                  atomsPerResidue = NA,
                  names = NA,
                  rangeAtoms = NA,
                  legendPosition = "topright",
                  barePlot = FALSE,
                  ... )
{
  # set colours and names
  PALETTE_RMSF_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( all( is.na( colours ) ) )
    colours <- PALETTE_RMSF_colours( length( rmsfData ) / 2 )
  if( all( is.na( names ) ) )
    names <- 1:( length( rmsfData ) / 2 )
  #########
  
  # get boundaries
  REAL_maxRMSF = max( unlist( lapply( rmsfData[ c( F, T ) ],
                                      FUN = function( x ) max( x ) ) ) )
  INT_minAtomnumber = min( unlist( lapply( rmsfData[ c( T, F ) ],
                                           FUN = function( x ) min( x ) ) ) )
  INT_maxAtomnumber = max( unlist( lapply( rmsfData[ c( T, F ) ],
                                           FUN = function( x ) max( x ) ) ) )
  if( all( is.na( rangeAtoms ) ) )
    rangeAtoms <- c( INT_minAtomnumber - 1,
                     INT_maxAtomnumber )
  #########
  
  # plot the RMSF for all elements of the list containing the data
  for( i in 1:length( rmsfData ) )
  {
    if( i %% 2 == 1 )
    {
      if( i == 1 )
        plot( rmsfData[[ i ]], rmsfData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n",
              yaxt = ifelse( barePlot, "n", "s" ),
              xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF * 1.05 ), xlim = rangeAtoms,
              ... )
      else
        plot( rmsfData[[ i ]], rmsfData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", yaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF * 1.05 ), xlim = rangeAtoms )
      par( new = TRUE )
    }
  }
  #########
  
  # plot axis labels and ticks, which are calculated either atom- or residuewise
  if( !barePlot )
  {
    mtext( side = 2, text = paste( "RMSF [", rmsfUnit, "]", sep = "" ), line = 2.4, cex = 1.25 )
    if( !residuewise )
    {
      mtext( side = 1, text = "atom number", line = 3, cex = 1.25 )
      VEC_atomTicks <- axTicks( 1,
                                usr = rangeAtoms )
      axis( 1,
            at = VEC_atomTicks,
            labels = VEC_atomTicks )
    }
    else
    {
      mtext( side = 1, text = "residue number", line = 3, cex = 1.25 )
      VEC_atomTicks <- axTicks( 1,
                                usr = rangeAtoms )
      VEC_labelTicks <- VEC_atomTicks
      if( !is.na( atomsPerResidue ) )
        VEC_labelTicks <- round( VEC_labelTicks / atomsPerResidue,
                                 digits = 0 )
      axis( 1,
            at = VEC_atomTicks,
            labels = VEC_labelTicks )
    }
  }
  #########
  
  # plot the rest
  if( printLegend && !barePlot )
    legend( legendPosition,
            title = "Legend",
            legend = names,
            col = colours,
            lty = 1.0, lwd = 2.0,
            cex = 1.0 )
  #########
  
  return( rmsfData )
}