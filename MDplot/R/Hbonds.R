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
                                     REAL_scalingFactorPlot = NULL,
                                     BOOL_printNames = FALSE,
                                     BOOL_namesToSingle = FALSE,
                                     BOOL_timeInNS = FALSE,
                                     REAL_divisionFactor = 1000,
                                     VEC_timeRange = NULL,
                                     VEC_hbondIndices = NULL,
                                     ... )
{
  
  # set all ranges to full, in case they are not specified
  if( is.null( VEC_acceptorRange ) )
    VEC_acceptorRange <- c( min( TABLE_summary[ , 4 ] ), max( TABLE_summary[ , 4 ] ) )
  if( is.null( VEC_donorRange ) )
    VEC_donorRange <- c( min( TABLE_summary[ , 2 ] ), max( TABLE_summary[ , 2 ] ) )
  if( is.null( VEC_hbondIndices ) )
    VEC_hbondIndices <- c( min( TABLE_summary[ , 1 ] ), max( TABLE_summary[ , 1 ] ) )
  #########
  
  # select the hbond-IDs of those hbonds, that match all criteria
  VEC_hbondIDs <- c()
  for( i in 1:nrow( TABLE_summary ) )
  {
    if( TABLE_summary[ i, 2 ] >= VEC_donorRange[ 1 ] &&
        TABLE_summary[ i, 2 ] <= VEC_donorRange[ 2 ] &&
        TABLE_summary[ i, 4 ] >= VEC_acceptorRange[ 1 ] &&
        TABLE_summary[ i, 4 ] <= VEC_acceptorRange[ 2 ] &&
        TABLE_summary[ i, 1 ] >= VEC_hbondIndices[ 1 ] &&
        TABLE_summary[ i, 1 ] <= VEC_hbondIndices[ 2 ] )
      VEC_hbondIDs <- c( VEC_hbondIDs, i )
  }
  #########

  # check, that one or more hbonds match the criteria
  # remove all hbonds from the timeseries table, that do not match the criteria
  if( length( VEC_hbondIDs ) == 0 )
    stop( paste( "The selection of acceptor residues ", VEC_acceptorRange[ 1 ], ":",
                 VEC_acceptorRange[ 2 ], " with donor residues ", VEC_donorRange[ 1 ], ":",
                 VEC_donorRange[ 2 ], " does not contain any hbonds.", sep = "" ) )
  TABLE_timeseries <- TABLE_timeseries[ ( TABLE_timeseries[ , 2 ] %in% VEC_hbondIDs ), ,
                                        drop = FALSE ]
  #########

  # set time axis
  VEC_timeLimits = c( min( TABLE_timeseries[ , 1 ] ),
                      max( TABLE_timeseries[ , 1 ] ) )
  VEC_timeTicks <- VEC_timeLimits
  if( BOOL_timeInNS )
    VEC_timeTicks <- VEC_timeTicks / REAL_divisionFactor
  #########

  # get the vector for the names (left plot)
  # in case, names are selected transform the hbond-ID into three or two letter constructs
  VEC_hbondNames <- split_equidistant( VEC_values = c( min( VEC_hbondIDs ),
                                                       max( VEC_hbondIDs ) ),
                                       n = 5 )
  VEC_hbondNamesPositions <- VEC_hbondNames
  if( BOOL_printNames )
  {
    VEC_hbondNames <- c()
    VEC_hbondNamesPositions <- VEC_hbondIDs
    for( i in 1:length( VEC_hbondIDs ) )
    {
      LIST_tableLine <- TABLE_summary[ TABLE_summary[ , 1 ] == VEC_hbondIDs[ i ], ]
      if( BOOL_namesToSingle )
      {
        LIST_tableLine[[ "ResDonorName" ]] <- translate_aminoacids( LIST_tableLine[[ "ResDonorName" ]],
                                                                    INT_switch = 1 )
        LIST_tableLine[[ "ResAcceptorName" ]] <- translate_aminoacids( LIST_tableLine[[ "ResAcceptorName" ] ],
                                                                       INT_switch = 1 ) 
      }
      VEC_hbondNames <- c( VEC_hbondNames,
                           paste( paste( LIST_tableLine[[ "ResDonorName" ]],
                                        LIST_tableLine[[ "ResDonor" ]],
                                        sep = "" ),
                                  "->",
                                  paste( LIST_tableLine[[ "ResAcceptorName" ]],
                                        LIST_tableLine[[ "ResAcceptor" ]],
                                        sep = "" ) ) )
    }
  }
  #########

  # calculate a scaling factor in dependence of the number of hbonds to be plotted
  if( is.null( REAL_scalingFactorPlot ) )
    REAL_scalingFactorPlot <- 0.75 / log( length( VEC_hbondIDs ) )
  #########

  # in case the occurences are to be plotted, make some space
  # do make sure, that the y axis has enough space for long labels
  par( mar = c( 4.0,
                ifelse( !BOOL_namesToSingle && BOOL_printNames,
                        7.95,
                        5.25 ),
                3.25,
                ifelse( BOOL_plotOccurences,
                        0.0,
                        2.0 ) ) )
  if( BOOL_plotOccurences )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
  #########
  
  # plot (left graph)
  plot( TABLE_timeseries,
        xlim = VEC_timeLimits, xaxs = "i", xaxt = "n", xlab = "",
        ylim = c( min( VEC_hbondIDs ), max( VEC_hbondIDs ) ), yaxt = "n", ylab = "",
        type = "n" )
  LIST_ellipsis <- list( ... )
  mtext( side = 3, line = 1.25, cex = 2.0,
         text = ifelse( is.null( LIST_ellipsis[[ "main" ]] ),
                        "Hbond timeseries",
                        LIST_ellipsis[[ "main" ]] ),
         adj = ifelse( BOOL_plotOccurences,
                       0.795,
                       0.5 ) )
  mtext( side = 1, line = 2.45, cex = 1.0, text = paste( "time",
                                                         ifelse( BOOL_timeInNS,
                                                                 "[ns]",
                                                                 "[snapshots]" ) ) )
  if( !BOOL_printNames )
    mtext( side = 2, line = 2.45, cex = 1, text = "hbonds [#]" )
  axis( 1,
        at = split_equidistant( VEC_values = VEC_timeLimits,
                                n = 6,
                                BOOL_removeLast = TRUE,
                                BOOL_roundDown = TRUE ),
        labels = split_equidistant( VEC_values = VEC_timeTicks,
                                    n = 6,
                                    BOOL_removeLast = TRUE,
                                    BOOL_roundDown = TRUE ) )
  axis( 2,
        at = VEC_hbondNamesPositions,
        label = VEC_hbondNames,
        las = ifelse( BOOL_printNames,
                      1,
                      0 ),
        cex.axis = ifelse( BOOL_printNames,
                           REAL_scalingFactorPlot * 2.45,
                           1 ) )
  segments( TABLE_timeseries[ , 1 ],
            TABLE_timeseries[ , 2 ] - REAL_scalingFactorPlot,
            TABLE_timeseries[ , 1 ],
            TABLE_timeseries[ , 2 ] + REAL_scalingFactorPlot,
            lwd = 0.15 )
  #########

  # in case the occurences are to be plotted, add them on the right hand side
  if( BOOL_plotOccurences )
  {
    VEC_occurences <- c()
    for( i in 1:length( VEC_hbondIDs ) )
    {
      INT_numberAppearances <- sum( TABLE_timeseries[ , 2 ] == VEC_hbondIDs[ i ] )
      
      # calculate percent from the time limits
      # CAUTION: might differ from the overall score out of the summary file
      VEC_occurences <- c( VEC_occurences, ( INT_numberAppearances / 
                                             ( VEC_timeLimits[ 2 ] - VEC_timeLimits[ 1 ] ) * 
                                               100 ) )
    }
    par( mar = c( 4.0, 0.0, 3.25, 3.0 ) )
    
    # plot
    plot( VEC_hbondIDs~VEC_occurences,
          ylim = c( min( VEC_hbondIDs ),
                    max( VEC_hbondIDs ) ),
          yaxt = "n", ylab = "",
          xaxs = "i", xlim = c( 0, 100 ), xlab = "",
          type = "n" )
    mtext( side = 1, line = 2.45, cex = 1, text = "occurence [%]" )
    segments( rep( 0, length( VEC_hbondIDs ) ),
              VEC_hbondIDs,
              VEC_occurences,
              VEC_hbondIDs,
              lwd = 2.0 )
    segments( VEC_occurences,
              VEC_hbondIDs - REAL_scalingFactorPlot * 0.65,
              VEC_occurences,
              VEC_hbondIDs + REAL_scalingFactorPlot * 0.65,
              lwd = 2.0 )
  }
  #########
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
      break
    LIST_buffer[[ INT_line ]] <- VEC_buffer
    INT_line <- INT_line + 1
  }
  close( CON_input )
  #########
  
  # transform list of lists into table using temporary file reads / writes and select
  # the important columns only
  DF_buffer <- data.frame( matrix( unlist( LIST_buffer ), ncol = 21, byrow = TRUE ),
                           stringsAsFactors = FALSE )
  TABLE_input <- DF_buffer[ , c( -20, -19, -18, -17, -15, -14, -12, -11, -7, -6, -3, -1 ) ]
  STRING_tempFile <- tempfile( "MDplot_Hbonds" )
  on.exit( unlink( STRING_tempFile, recursive = FALSE, force = FALSE ) )
  write.table( TABLE_input, file = STRING_tempFile )
  TABLE_input <- read.table( STRING_tempFile )
  #########
  
  # update column names
  colnames( TABLE_input ) <- c( "HbondID", "ResDonor", "ResDonorName",
                                "ResAcceptor", "ResAcceptorName", "AtomDonor",
                                "AtomH", "AtomAcceptor", "Percentage" )
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
    VEC_boundariesDonor <- c( min( TABLE_input[ , 2 ] ), max( TABLE_input[ , 2 ] ) )
    VEC_boundariesAcceptor <- c( min( TABLE_input[ , 4 ] ), max( TABLE_input[ , 4 ] ) )
    
    # identify, whether hbond belongs to already identified hbond and add
    # the percentage there
    for( i in 1:nrow( TABLE_input ) )
    {
      BOOL_found = FALSE
      if( nrow( TABLE_result ) != 0 )
      {
        for( j in 1:nrow( TABLE_result ) )
        {
          if( TABLE_result[ j, 2 ] == TABLE_input[ i, 2 ] &&
                TABLE_result[ j, 4 ] == TABLE_input[ i, 4 ] )
          {
            TABLE_result[ j, 9 ] <- TABLE_result[ j, 9 ] + TABLE_input[ i, 9 ]
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
    TABLE_result <- TABLE_result[ , c( -8, -7, -6, -5, -3, -1 ) ]
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
    if( BOOL_printLegend )
    {
      par( mar = c( 4.25, 4.25, 3.25, 0.25 ) )
      layout( matrix( 1:2, ncol = 2 ), width = c( 2.5, 0.5 ), height = c( 1.0, 1.0 ) )
    }
    PALETTE_colors <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
    PALETTE_colors_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    plot( TABLE_result[ , 1:2 ],
          col = PALETTE_colors_rev( 10 )[ as.numeric( cut( as.numeric( VEC_normalized ), breaks = 10 ) )  ],
          pch = 19,
          cex = 0.75,
          xlab = "Donor [residue number]",
          ylab = "Acceptor [residue number]",
          ... )
    if( BOOL_printLegend )
    {
      par( mar = c( 4.25, 0.25, 3.25, 1.0 ) )
      legend_image <- as.raster( matrix( PALETTE_colors( 10 ), ncol = 1 ) )
      plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = '[%]' )
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