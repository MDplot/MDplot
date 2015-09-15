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

MDplot_hbond <- function( TABLE_input, STRING_what_to_plot = "residue-wise", VEC_acceptorRange = NULL,
                          VEC_donorRange = NULL, BOOL_print_legend = TRUE )
{
  
  # residue-wise: sum all residue-to-residue hbonds up (per-atom contributions)
  if( STRING_what_to_plot == "residue-wise" )
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
          cex = 1.25 )
    if( BOOL_print_legend )
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
    stop( paste( "Error: plot method ", STRING_what_to_plot, " is unknown. Process aborted!" ) )
  }
}