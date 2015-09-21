# load RMSD
MDplot_load_RMSD <- function( VEC_files )
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

# plot RMSD
MDplot_RMSD <- function( LIST_datainput,
                         BOOL_frax = TRUE,
                         REAL_division_factor = 1,
                         xunit = "ns",
                         VEC_colours = NULL,
                         ... )
{
  # get boundaries
  REAL_max_RMSD = max( sapply( LIST_datainput[ c( F, T ) ], max ) )
  INT_max_atomnumber = max( sapply( LIST_datainput[ c( T, F ) ], max ) )
  #########
  
  # set colours
  PALETTE_RMSF_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.null( VEC_colours ) )
  {
    VEC_colours <- PALETTE_RMSF_colours( length( LIST_datainput ) / 2 )
  }
  #########
  
  # transpose and get rid of first column before you do
  # plot
  #TS_datainput <- as.ts( MAT_MDplot_RMSD_example[ , -1 ] )
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
            col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
            xaxt = "n",  xlab = "", ylab = "",
            ylim = c( 0, REAL_max_RMSD ), xlim = c( 0, INT_max_atomnumber ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot the rest
  #axis( 1,
  #      at = split_equidistant( c( 1, nrow( MAT_datainput ) ), 7 ),
  #      labels = split_equidistant( c( 1, ( nrow( MAT_datainput ) / REAL_division_factor ) ), 7 ),
  #      cex.axis = 1 )
  #mtext( side = 1, text = paste( "time [", xunit, "]" ), line = 3,
  #       cex = 1.75 )
  #mtext( side = 2, text = "RMSD", line = 2.4,
  #       cex = 1.75 )
  #title( main )
  #legend( "topright", legend = colnames( MAT_MDplot_RMSD_example[ , -1 ] ), col = COLOURS_RMSD, lty = 1, cex = 1 )
  #########
}