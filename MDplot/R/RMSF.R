# load the RMSF file(s)
MDplot_load_RMSF <- function( VEC_files )
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
MDplot_RMSF <- function( LIST_datainput,
                         xunit = "ns",
                         VEC_colours = NULL,
                         BOOL_residuewise = FALSE,
                         INT_number_xticks = 7, 
                         ... )
{
  # set colours
  PALETTE_RMSF_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.null( VEC_colours ) )
  {
    VEC_colours <- PALETTE_RMSF_colours( length( LIST_datainput ) / 2 )
  }
  #########
  
  # get boundaries
  REAL_max_RMSF = max( sapply( LIST_datainput[ c( F, T ) ], max ) )
  INT_max_atomnumber = max( sapply( LIST_datainput[ c( T, F ) ], max ) )
  #########
  
  # plot the RMSF for all elements of the list containing the data
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
            col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
            xaxt = "n",  xlab = "", ylab = "",
            ylim = c( 0, REAL_max_RMSF ), xlim = c( 0, INT_max_atomnumber ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot axis labels and ticks, which are calculated either atom- or residuewise
  mtext( side = 2, text = "RMSF", line = 2.4, cex = 1.75 )
  VEC_atomnumbers <- 0:INT_max_atomnumber
  if( !BOOL_residuewise )
  {
    mtext( side = 1, text = "atoms", line = 3, cex = 1.75 )
    axis( 1, at = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ),
          label = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ) )
  }
  else
  {
    mtext( side = 1, text = "residues", line = 3, cex = 1.75 )
    axis( 1, at = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ),
          label = as.integer( split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks )
                              / 3 ) )
  }
  #########
  
  # plot the rest
  #title( ... )
  #legend( "topright", legend = colnames( XYZ ),
  #        col = COLOURS_RMSF, lty = 1, cex = 1 )
  #########
}