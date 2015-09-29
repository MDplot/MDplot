# load and parse GROMOS hbond timeseries
MDplot_load_hbond_timeseries <- function( STRING_path )
{
  TABLE_timeseries <- read.table( STRING_path )
  return( TABLE_timeseries )
}

# plot hbond timeseries
MDplot_hbond_timeseries <- function( TABLE_timeseries,
                                     TABLE_summary,
                                     VEC_acceptorRange = NULL,
                                     VEC_donorRange = NULL,
                                     BOOL_plotOccurences = FALSE,
                                     REAL_heightsHbonds = NULL )
{
  if( is.null( VEC_acceptorRange ) )
    VEC_acceptorRange = c( min( TABLE_summary[ , 3 ] ), max( TABLE_summary[ , 3 ] ) )
  if( is.null( VEC_donorRange ) )
    VEC_donorRange = c( min( TABLE_summary[ , 1 ] ), max( TABLE_summary[ , 1 ] ) )
  VEC_hbondIDs <- c()
  for( i in 1:nrow( TABLE_summary ) )
  {
    if( TABLE_summary[ i, 1 ] >= VEC_donorRange[ 1 ] &&
        TABLE_summary[ i, 1 ] <= VEC_donorRange[ 2 ] &&
        TABLE_summary[ i, 3 ] >= VEC_acceptorRange[ 1 ] &&
        TABLE_summary[ i, 3 ] <= VEC_acceptorRange[ 2 ] )
      VEC_hbondIDs <- c( VEC_hbondIDs, i )
  }
  if( length( VEC_hbondIDs ) == 0 )
    stop( paste( "The selection, acceptor residues ", VEC_acceptorRange[ 1 ], ":",
                 VEC_acceptorRange[ 2 ], " with donor residues ", VEC_donorRange[ 1 ], ":",
                 VEC_donorRange[ 2 ], " does not contain any hbonds.", sep = "" ) )
  VEC_timeLimits = c( min( TABLE_timeseries[ , 1 ] ),
                   max( TABLE_timeseries[ , 1 ] ) )
  TABLE_timeseries <- TABLE_timeseries[ ( TABLE_timeseries[ , 2 ] %in% VEC_hbondIDs ), ,
                                        drop = FALSE ]
  if( is.null( REAL_heightsHbonds ) )
    REAL_heightsHbonds <- 0.75 / log( length( VEC_hbondIDs ) )
  if( BOOL_plotOccurences )
  {
    par( mar = c( 3.0, 3.0, 3.0, 0.0 ) )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
  }
  plot( TABLE_timeseries,
        xlim = VEC_timeLimits,
        ylim = c( min( VEC_hbondIDs ), max( VEC_hbondIDs ) ),
        type = "n" )
  segments( TABLE_timeseries[ , 1 ],
            TABLE_timeseries[ , 2 ] - REAL_heightsHbonds,
            TABLE_timeseries[ , 1 ],
            TABLE_timeseries[ , 2 ] + REAL_heightsHbonds,
            lwd = 0.15 )
  if( BOOL_plotOccurences )
  {
    VEC_occurences <- c()
    for( i in 1:length( VEC_hbondIDs ) )
    {
      INT_numberAppearances <- sum( TABLE_timeseries[ , 2 ] == VEC_hbondIDs[ i ] )
      VEC_occurences <- c( VEC_occurences, ( INT_numberAppearances / 
                                             ( VEC_timeLimits[ 2 ] - VEC_timeLimits[ 1 ] ) * 
                                               100 ) )
    }
    par( mar = c( 3.0, 0.0, 3.0, 3.0 ) )
    plot( VEC_hbondIDs~VEC_occurences,
          ylab = "", yaxt = "n", ylim = c( min( VEC_hbondIDs ),
                                           max( VEC_hbondIDs ) ),
          xaxs = "i", xlim = c( 0, 100 ),
          type = "n" )
    segments( rep( 0, length( VEC_hbondIDs ) ),
              VEC_hbondIDs,
              VEC_occurences,
              VEC_hbondIDs,
              lwd = 2.0 )
    segments( VEC_occurences,
              VEC_hbondIDs - REAL_heightsHbonds * 0.75,
              VEC_occurences,
              VEC_hbondIDs + REAL_heightsHbonds * 0.75,
              lwd = 2.0 )
  }
}

# load and parse GROMOS hbond output
MDplot_load_hbond <- function( STRING_path )
{
  CON_input <- file( STRING_path, open = "r" )
  
  #skip first comments
  readLines( CON_input, n = 23, warn = FALSE )
  LIST_buffer <- list()
  INT_line <- 1
  #########
  
  # read line by line and split by one or multiple spaces
  # second block (three-centered hbonds) is skipped
  while( length( STRING_theLine <- readLines( CON_input, n = 1, warn = FALSE ) ) > 0 )
  {
    VEC_buffer <- unlist( strsplit( STRING_theLine, split = " +" ) )
    if( length( VEC_buffer ) == 0 )
    {
      break
    }
    LIST_buffer[[ INT_line ]] <- VEC_buffer
    INT_line <- INT_line + 1
  }
  close( CON_input )
  #########
  
  # transform list of lists into table using temporary file reads / writes and select
  # the important columns only
  DF_buffer <- data.frame( matrix( unlist( LIST_buffer ), ncol = 21, byrow = TRUE ),
                           stringsAsFactors = FALSE )
  TABLE_input <- DF_buffer[ , c( -20, -19, -18, -17, -15, -14, -12, -11, -7, -6, -3, -2, -1 ) ]
  STRING_tempFile <- tempfile( "MDplot_Hbonds" )
  on.exit( unlink( STRING_tempFile, recursive = FALSE, force = FALSE ) )
  write.table( TABLE_input, file = STRING_tempFile )
  TABLE_input <- read.table( STRING_tempFile )
  #########
  
  # update column names
  colnames( TABLE_input ) <- c( "ResDonor", "ResDonorName", "ResAcceptor",
                                "ResAcceptorName", "AtomDonor", "AtomH",
                                "AtomAcceptor", "Percentage" )
  #########
  return( TABLE_input )
}

# plot the hbond information
MDplot_hbond <- function( TABLE_input,
                          STRING_plotMethod = "residue-wise",
                          VEC_acceptorRange = NULL,
                          VEC_donorRange = NULL,
                          BOOL_printLegend = TRUE,
                          ... )
{
  
  # residue-wise: sum all residue-to-residue hbonds up (per-atom contributions)
  if( STRING_plotMethod == "residue-wise" )
  {
    TABLE_result <- as.data.frame( NULL )
    VEC_boundariesDonor <- c( min( TABLE_input[ , 1 ] ), max( TABLE_input[ , 1 ] ) )
    VEC_boundariesAcceptor <- c( min( TABLE_input[ , 3 ] ), max( TABLE_input[ , 3 ] ) )
    
    # identify, whether hbond belongs to already identified hbond and add
    # the percentage there
    for( i in 1:nrow( TABLE_input ) )
    {
      BOOL_found = FALSE
      if( nrow( TABLE_result ) != 0 )
      {
        for( j in 1:nrow( TABLE_result ) )
        {
          if( TABLE_result[ j, 1 ] == TABLE_input[ i, 1 ] &&
                TABLE_result[ j, 3 ] == TABLE_input[ i, 3 ] )
          {
            TABLE_result[ j, 8 ] <- TABLE_result[ j, 8 ] + TABLE_input[ i, 8 ]
            BOOL_found = TRUE
            break
          }
        }
      }
      if( !BOOL_found )
      {
        TABLE_result <- rbind( TABLE_result, TABLE_input[ i, ] )
      }
    }
    #########
    
    # remove unused columns and define zoom area if specified
    TABLE_result <- TABLE_result[ , c( -7, -6, -5, -4, -2 ) ]
    TABLE_result <- TABLE_result[ order( TABLE_result[ , 1 ], TABLE_result[ , 2 ] ), ]
    PALETTE_residuewise <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    if( !is.null( VEC_acceptorRange ) )
    {
      TABLE_result <- TABLE_result[ TABLE_result[ , 2 ] >= VEC_acceptorRange[ 1 ], , drop = FALSE ]
      TABLE_result <- TABLE_result[ TABLE_result[ , 2 ] <= VEC_acceptorRange[ 2 ], , drop = FALSE ]
    }
    if( !is.null( VEC_donorRange ) )
    {
      TABLE_result <- TABLE_result[ TABLE_result[ , 1 ] >= VEC_donorRange[ 1 ], , drop = FALSE ]
      TABLE_result <- TABLE_result[ TABLE_result[ , 1 ] <= VEC_donorRange[ 2 ], , drop = FALSE ]
    }
    #########
    
    # normalize values to 0 to 100% and plot graph and legend (in case)
    VEC_normalized <- lapply( as.list( TABLE_result[ , 3 ] ), function( x ) x / 100 )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
    PALETTE_colors <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
    PALETTE_colors_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    plot( TABLE_result[ , 1:2 ],
          col = PALETTE_colors_rev( 10 )[ as.numeric( cut( as.numeric( VEC_normalized ), breaks = 10 ) )  ],
          pch = 19,
          cex = 1.00,
          ... )
    if( BOOL_printLegend )
    {
      legend_image <- as.raster( matrix( PALETTE_colors( 10 ), ncol = 1 ) )
      plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = 'Color legend [%]' )
      text( x = 1.5, y = seq( 0, 1, l = 5 ),
            labels = seq( 0, 100, l = 5 ) )
      rasterImage( legend_image, 0, 0, 1, 1 )
    }
    #########
  }
  else
  {
    stop( paste( "Error: plot method ", STRING_plotMethod, " is unknown. Process aborted!" ) )
  }
}