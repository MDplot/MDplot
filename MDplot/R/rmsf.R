# load the RMSF file(s)
load_rmsf <- function( files )
{
  LIST_return <- list()
  for( i in 1:length( files ) )
  {
    TABLE_input <- read.table( files[ i ] )
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
                  numberXLabels = 7,
                  names = NA,
                  ... )
{
  # set colours and names
  PALETTE_RMSF_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.na( colours ) )
    colours <- PALETTE_RMSF_colours( length( rmsfData ) / 2 )
  if( is.na( names ) )
    names <- 1:( length( rmsfData ) / 2 )
  #########
  
  # get boundaries
  REAL_maxRMSF = max( unlist( lapply( rmsfData[ c( F, T ) ],
                                      FUN = function( x ) max( x ) ) ) )
  INT_maxAtomnumber = max( unlist( lapply( rmsfData[ c( T, F ) ],
                                           FUN = function( x ) max( x ) ) ) )
  #########
  
  # plot the RMSF for all elements of the list containing the data
  for( i in 1:length( rmsfData ) )
  {
    if( i %% 2 == 1 )
    {
      if( i == 1 )
        plot( rmsfData[[ i ]], rmsfData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF * 1.05 ), xlim = c( 0, INT_maxAtomnumber ),
              ... )
      else
        plot( rmsfData[[ i ]], rmsfData[[ ( i + 1 ) ]], type = "l",
              col = colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", yaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF * 1.05 ), xlim = c( 0, INT_maxAtomnumber ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot axis labels and ticks, which are calculated either atom- or residuewise
  mtext( side = 2, text = paste( "RMSF [", rmsfUnit, "]", sep = "" ), line = 2.4, cex = 1.25 )
  VEC_atomNumbers <- 0:INT_maxAtomnumber
  if( !residuewise )
  {
    mtext( side = 1, text = "atom number", line = 3, cex = 1.25 )
    axis( 1, at = split_equidistant( c( 0, INT_maxAtomnumber ), numberXLabels ),
          label = split_equidistant( c( 0, INT_maxAtomnumber ), numberXLabels ) )
  }
  else
  {
    mtext( side = 1, text = "residue number", line = 3, cex = 1.25 )
    axis( 1, at = split_equidistant( c( 0, INT_maxAtomnumber ), numberXLabels ),
          label = as.integer( split_equidistant( c( 0, INT_maxAtomnumber ), numberXLabels )
                              / 3 ) )
  }
  #########
  
  # plot the rest
  if( printLegend )
    legend( "topright",
            title = "Legend",
            legend = names,
            col = colours,
            lty = 1.0, lwd = 2.0,
            cex = 1.0 )
  #########
}