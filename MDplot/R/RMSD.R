# plot the average RMSD of many runs with spread
MDplot_RMSD_average <- function( LIST_input,
                                 INT_skip = 0,
                                 ... )
{
  
  # initialization
  VEC_names <- rep( NA, length( LIST_input ) )
  MAT_result <- NULL
  #########
  
  # calculate the average RMSD and the standard deviation
  for( i in 1:length( LIST_input ) )
  {
    VEC_names[ i ] <- LIST_input[[ i ]][[ "name" ]]
    VEC_files <- LIST_input[[ i ]][[ "files" ]]
    VEC_values <- c()
    for( j in 1:length( VEC_files ) )
    {
      VEC_buffer <- read.table( VEC_files[ j ] )[ , 2 ]
      if( INT_skip > 0 )
        VEC_buffer <- VEC_buffer[ INT_skip:length( VEC_buffer ) ]
      VEC_values <- c( VEC_values, VEC_buffer )
    }
    MAT_result <- rbind( MAT_result, c( mean( VEC_values ), sd( VEC_values ) ) )
  }
  #########
  
  # set the column and row names
  colnames( MAT_result ) <- c( "values", "sds" )
  rownames( MAT_result ) <- VEC_names
  #########
  
  # plot the bars and the errors
  par( mar = c( 2.5, 4.25, 1.0, 1.5 ) )
  PLOT_positions = barplot( MAT_result[ , 1 ],
                            ylim = c( 0.0, 1.5 * max( MAT_result[ , 1 ] ) ),
                            ylab = "RMSD [nm]",
                            xaxt = "n",
                            ... )
  plot_segments( cbind( PLOT_positions, MAT_result[ , 1 ] ),
                 VEC_spread = MAT_result[ , 2 ] )
  axis( 1,
        at = PLOT_positions,
        labels = rownames( MAT_result ),
        cex.axis = 0.95,
        tick = FALSE )
  #########
}

# load RMSD
MDplot_load_RMSD <- function( VEC_files )
{
  LIST_return <- list()
  for( i in 1:length( VEC_files ) )
  {
    TABLE_input <- read.table( VEC_files[ i ] )
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
MDplot_RMSD <- function( LIST_datainput,
                         BOOL_printLegend = TRUE,
                         BOOL_frax = TRUE,
                         REAL_divisionFactor = 1000,
                         STRING_timeUnit = "ns",
                         STRING_RMSDUnit = "nm",
                         VEC_colours = NA,
                         VEC_names = NA,
                         ... )
{
  # get boundaries
  REAL_max_RMSD = max( unlist( lapply( LIST_datainput,
                                       FUN = function( x ) max( x[[ 2 ]] ) ) ) )
  INT_max_atomnumber = max( unlist( lapply( LIST_datainput,
                                            FUN = function( x ) max( x[[ 1 ]] ) ) ) )
  #########
  
  # set colours and names
  PALETTE_RMSD_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.na( VEC_colours ) )
    VEC_colours <- PALETTE_RMSD_colours( length( LIST_datainput ) / 2 )
  if( is.na( VEC_names ) )
    VEC_names = 1:( length( LIST_datainput ) / 2 )
  #########
  
  # transpose and get rid of first column before you do
  # plot
  #TS_datainput <- as.ts( MAT_MDplot_RMSD_example[ , -1 ] )
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      if( i == 1 )
        plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
              col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n",  xlab = "", ylab = "",
              ylim = c( 0, REAL_max_RMSD ), xlim = c( 0, INT_max_atomnumber ), ... )
      else
        plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]], type = "l",
              col = VEC_colours[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
              xaxt = "n", yaxt = "n", xlab = "", ylab = "",
              ylim = c( 0, REAL_max_RMSD ), xlim = c( 0, INT_max_atomnumber ) )
      par( new = TRUE )
    }
  }
  #########
  
  # plot the rest
  axis( 1,
        at = split_equidistant( c( 1, length( LIST_datainput[[ 1 ]] ) ), 7 ),
        labels = split_equidistant( c( 1, ( length( LIST_datainput[[ 1 ]] ) / REAL_divisionFactor ) ), 7 ),
        cex.axis = 1 )
  mtext( side = 1, text = paste( "time [", STRING_timeUnit, "]", sep = "" ), line = 3,
         cex = 1.25 )
  mtext( side = 2, text = paste( "RMSD [", STRING_RMSDUnit, "]", sep = "" ), line = 2.4,
         cex = 1.25 )
  if( BOOL_printLegend )
    legend( "bottomright",
            title = "Legend",
            legend = VEC_names,
            col = VEC_colours,
            lty = 1.0, lwd = 2.0,
            cex = 1.0 )
  #########
}