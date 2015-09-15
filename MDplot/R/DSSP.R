MDplot_DSSP_summary <- function( TABLE_datainput, BOOL_printLegend = FALSE, COLOURS_DSSP_summary = NULL, VEC_showValues = NULL,
                                 VEC_show_residues = NULL )
{
  VEC_residues <- TABLE_datainput[ , 1 ]
  TABLE_datainput <- TABLE_datainput[ , -1 ]
  MAT_data <- as.matrix( TABLE_datainput[ , c( F, T ) ] )
  MAT_buffer <- MAT_data
  if( !is.null( VEC_show_residues ) )
  {
    for( i in nrow( MAT_buffer ):1 )
    {
      if( !( i %in% VEC_show_residues ) )
      {
        MAT_buffer <- MAT_buffer[ -i, , drop = FALSE ]
      }
    }
  }
  MAT_data <- MAT_buffer
  if( is.null( COLOURS_DSSP_summary ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    COLOURS_DSSP_summary <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  if( is.null( VEC_showValues  ) )
  {
    VEC_showValues = rep( 1:ncol( MAT_data ) )
  }
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
  {
    if( !( i %in% VEC_showValues ) )
    {
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
    }
  }
  MAT_data <- MAT_buffer
  if( BOOL_printLegend )
  {
    par( mar = c( 4.5, 4.5, 2.5, 12 ) )
  }
  else
  { 
    par( mar = c( 4.5, 4.5, 2.5, 2.5 ) )
  }
  plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
        xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), col = COLOURS_DSSP_summary, 
        xlab = "residues", ylab = "fractions [%]",  xaxs = "i", yaxs = "i", 
        cex = 0.75, pch = 19 )
  if( nrow( MAT_data ) > 1 )
  {
    for( i in 2:nrow( MAT_data ) )
    {
      par( new = TRUE )
      plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ), MAT_data[ i, ], col = COLOURS_DSSP_summary, 
            xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
            xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), cex = 0.75, pch = 19 )
    }
  }
  if( BOOL_printLegend )
  {
    par( xpd = TRUE )
    legend( 110, 75, legend = colnames( MAT_data ), col = COLOURS_DSSP_summary, lty = 1, cex = 1 )
    par( xpd = FALSE )
  }
}

MDplot_DSSP_timeseries <- function( VEC_timeseries )
{
  
}