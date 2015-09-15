MDplot_load_rmsf <- function( VEC_files )
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

MDplot_RMSF <- function( LIST_datainput, plotTitle = "RMSF plot", xunit = "ns", 
                         VEC_colors = NULL, BOOL_residuewise = FALSE, INT_number_xticks = 7, 
                         ... )
{
  PALETTE_RMSF_colors <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.null( VEC_colors ) )
  {
    VEC_colors <- PALETTE_RMSF_colors( length( LIST_datainput ) / 2 )
  }
  par( new = FALSE )
  REAL_max_RMSF = max( sapply( LIST_datainput[ c( F, T ) ], max ) )
  INT_max_atomnumber = max( sapply( LIST_datainput[ c( T, F ) ], max ) )
  # find better solution for "every second column as i"
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]],
            type = "l", col = VEC_colors[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
            xaxt = "n",  xlab = "", ylab = "",
            ylim = c( 0, REAL_max_RMSF ),
            xlim = c( 0, INT_max_atomnumber ),
            ... )
      par( new = TRUE )
    }
  }
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
  title( plotTitle )
  #legend( "topright", legend = colnames( XYZ ),
  #        col = COLOURS_RMSF, lty = 1, cex = 1 )
}