# load the RMSF file(s)
load_rmsf <- function( VEC_files )
{
  LIST_return <- list()
  for( i in 1:length( VEC_files ) )
  {
    TABLE_input <- read.table( VEC_files[ i ] )
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
rmsf <- function( LIST_datainput,
                  BOOL_printLegend = TRUE,
                  STRING_RMSFUnit = "nm",
                  VEC_colours = NA,
                  BOOL_residueWise = FALSE,
                  INT_numberXLabels = 7,
                  VEC_names = NA,
                  ... )
{
  # set colours and names
  PALETTE_RMSF_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.na( VEC_colours ) )
    VEC_colours <- PALETTE_RMSF_colours( length( LIST_datainput ) / 2 )
  if( is.na( VEC_names ) )
    VEC_names <- 1:( length( LIST_datainput ) / 2 )
  #########
  
  # get boundaries
  REAL_maxRMSF = max( unlist( lapply( LIST_datainput,
                                      FUN = function( x ) max( x[[ 2 ]] ) ) ) )
  INT_maxAtomnumber = max( unlist( lapply( LIST_datainput,
                                           FUN = function( x ) max( x[[ 1 ]] ) ) ) )
  #########
  
  # plot the RMSF for all elements of the list containing the data
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      if( i == 1 )
        plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
              col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF ), xlim = c( 0, INT_maxAtomnumber ),
              ... )
      else
        plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
              col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", yaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_maxRMSF ), xlim = c( 0, INT_maxAtomnumber ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot axis labels and ticks, which are calculated either atom- or residuewise
  mtext( side = 2, text = paste( "RMSF [", STRING_RMSFUnit, "]", sep = "" ), line = 2.4, cex = 1.25 )
  VEC_atomNumbers <- 0:INT_maxAtomnumber
  if( !BOOL_residueWise )
  {
    mtext( side = 1, text = "atom number", line = 3, cex = 1.25 )
    axis( 1, at = split_equidistant( c( 0, INT_maxAtomnumber ), INT_numberXLabels ),
          label = split_equidistant( c( 0, INT_maxAtomnumber ), INT_numberXLabels ) )
  }
  else
  {
    mtext( side = 1, text = "residue number", line = 3, cex = 1.25 )
    axis( 1, at = split_equidistant( c( 0, INT_maxAtomnumber ), INT_numbeXLabels ),
          label = as.integer( split_equidistant( c( 0, INT_maxAtomnumber ), INT_numberXLabels )
                              / 3 ) )
  }
  #########
  
  # plot the rest
  if( BOOL_printLegend )
    legend( "topright",
            title = "Legend",
            legend = VEC_names,
            col = VEC_colours,
            lty = 1.0, lwd = 2.0,
            cex = 1.0 )
  #########
}